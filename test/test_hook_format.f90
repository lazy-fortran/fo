program test_hook_format
    ! Changed-line formatting through the pre-commit hook and fo fmt --changed.
    ! Kept apart from test_hook_precommit, whose process-boundary cases are slow.
    use fo_test_harness, only: string_list_t, process_result_t, list_add, &
        make_scratch, join_path, write_text, read_text, remove_tree, &
        assert_contains, assert_equal_string, assert_true, assert_process_ok, &
        finish_assertions
    use fo_test_cli, only: resolve_driver, run_fo, run_external
    implicit none
    character(len=*), parameter :: nl = new_line('a')
    character(len=:), allocatable :: driver, scratch, project
    type(process_result_t) :: result
    type(string_list_t) :: args

    call resolve_driver(driver)
    call make_scratch('fo-hook-format', scratch)
    ! A cold build on a loaded host can use up the default hook budget.
    call write_text(scratch//'/hook-gitconfig', '[fo]'//nl//'    hookBudget = 600'//nl)

    ! Legacy formatting on lines a commit does not touch never blocks it; the
    ! changed lines are still checked.
    project = join_path(scratch, 'legacy-format')
    call write_text(project//'/fpm.toml', 'name = "hook_legacy"'//nl)
    call write_text(project//'/src/legacy.f90', 'module legacy_mod'//nl// &
        'implicit none'//nl//'    integer, parameter :: answer = 42'//nl// &
        'end module legacy_mod'//nl)
    call git([character(len=64) :: 'init', '-q'])
    call git([character(len=64) :: 'add', 'fpm.toml', 'src/legacy.f90'])
    call git([character(len=64) :: 'commit', '-qm', 'Synthetic legacy source'])
    call write_text(project//'/src/legacy.f90', 'module legacy_mod'//nl// &
        'implicit none'//nl//'    integer, parameter :: answer = 43'//nl// &
        'end module legacy_mod'//nl)
    call git([character(len=64) :: 'add', 'src/legacy.f90'])
    call hook()
    call assert_process_ok(result, 'clean change beside legacy formatting passes')
    call write_text(project//'/src/legacy.f90', 'module legacy_mod'//nl// &
        'implicit none'//nl//'      integer,parameter::answer=43'//nl// &
        'end module legacy_mod'//nl)
    call git([character(len=64) :: 'add', 'src/legacy.f90'])
    call hook()
    call assert_true(result%exit_code /= 0, 'badly formatted changed line blocks')
    call assert_contains(result%stdout, 'legacy.f90:3: needs formatting', &
        'changed line is reported')
    ! fmt --changed applies exactly what the check asks for: the changed line
    ! takes its neighbour's indentation, and the unindented legacy line beside
    ! it stays as it is.
    args = string_list_t()
    call list_add(args, 'fmt')
    call list_add(args, '--changed')
    call run_fo(driver, args, project, scratch//'/cache', result, timeout_ms=60000)
    call assert_process_ok(result, 'fmt --changed formats the changed line')
    call assert_equal_string(read_text(project//'/src/legacy.f90'), &
        'module legacy_mod'//nl//'implicit none'//nl// &
        'integer,parameter::answer=43'//nl//'end module legacy_mod'//nl, &
        'fmt --changed rewrites only the changed line')
    call git([character(len=64) :: 'add', 'src/legacy.f90'])
    call hook()
    call assert_process_ok(result, 'hook accepts what fmt --changed wrote')

    ! A merge is judged by its own resolutions: lines that arrive unchanged from
    ! either parent never block it, a line differing from both parents does.
    project = join_path(scratch, 'merge-format')
    call write_text(project//'/fpm.toml', 'name = "hook_merge"'//nl)
    ! The unchanged middle line keeps the two branches' edits apart, so the
    ! merge itself does not conflict.
    call write_text(project//'/src/merged.f90', 'module merged_mod'//nl// &
        'implicit none'//nl//'    integer, parameter :: a = 1'//nl// &
        '    integer, parameter :: c = 0'//nl// &
        '    integer, parameter :: b = 2'//nl//'end module merged_mod'//nl)
    call git([character(len=64) :: 'init', '-q'])
    call git([character(len=64) :: 'add', 'fpm.toml', 'src/merged.f90'])
    call git([character(len=64) :: 'commit', '-qm', 'Synthetic merge base'])
    call git([character(len=64) :: 'checkout', '-q', '-b', 'side'])
    call write_text(project//'/src/merged.f90', 'module merged_mod'//nl// &
        'implicit none'//nl//'      integer,parameter::a=1'//nl// &
        '    integer, parameter :: c = 0'//nl// &
        '    integer, parameter :: b = 2'//nl//'end module merged_mod'//nl)
    call git([character(len=64) :: 'commit', '-qam', 'Side branch with its own formatting'])
    call git([character(len=64) :: 'checkout', '-q', '-'])
    call write_text(project//'/src/merged.f90', 'module merged_mod'//nl// &
        'implicit none'//nl//'    integer, parameter :: a = 1'//nl// &
        '    integer, parameter :: c = 0'//nl// &
        '    integer, parameter :: b = 3'//nl//'end module merged_mod'//nl)
    call git([character(len=64) :: 'commit', '-qam', 'Main branch change'])
    call git([character(len=64) :: 'merge', '-q', '--no-ff', '--no-commit', 'side'])
    call hook()
    call assert_process_ok(result, 'lines from either merge parent pass')
    call write_text(project//'/src/merged.f90', 'module merged_mod'//nl// &
        'implicit none'//nl//'      integer,parameter::a=1'//nl// &
        '    integer, parameter :: c = 0'//nl// &
        '      integer,parameter::b=4'//nl//'end module merged_mod'//nl)
    call git([character(len=64) :: 'add', 'src/merged.f90'])
    call hook()
    call assert_true(result%exit_code /= 0, 'badly formatted merge resolution blocks')
    call assert_contains(result%stdout, 'merged.f90:5: needs formatting', &
        'merge resolution is reported')

    ! A changed line follows its file's own indentation width.
    project = join_path(scratch, 'legacy-indent')
    call write_text(project//'/src/indent.f90', two_space_module('1', '    '))
    call git([character(len=64) :: 'init', '-q'])
    call git([character(len=64) :: 'add', 'src/indent.f90'])
    call git([character(len=64) :: 'commit', '-qm', 'Synthetic two-space source'])
    call write_text(project//'/fpm.toml', 'name = "hook_indent"'//nl)
    call write_text(project//'/src/indent.f90', two_space_module('2', '    '))
    call git([character(len=64) :: 'add', 'fpm.toml', 'src/indent.f90'])
    call hook()
    call assert_process_ok(result, 'change in a two-space file passes')
    call write_text(project//'/src/indent.f90', two_space_module('2', '  '))
    call git([character(len=64) :: 'add', 'src/indent.f90'])
    call hook()
    call assert_true(result%exit_code /= 0, 'misindented changed line blocks')
    call assert_contains(result%stdout, 'indent.f90:6: needs formatting', &
        'misindented line is reported')

    ! A file whose procedures sit at column 0 after `contains` keeps that style,
    ! and a stray three-space line does not set its indentation width.
    project = join_path(scratch, 'flat-contains')
    call write_text(project//'/src/flat.f90', flat_module('1', '        '))
    call git([character(len=64) :: 'init', '-q'])
    call git([character(len=64) :: 'add', 'src/flat.f90'])
    call git([character(len=64) :: 'commit', '-qm', 'Synthetic column-0 procedures'])
    call write_text(project//'/fpm.toml', 'name = "hook_flat"'//nl)
    call write_text(project//'/src/flat.f90', flat_module('2', '        '))
    call git([character(len=64) :: 'add', 'fpm.toml', 'src/flat.f90'])
    call hook()
    call assert_process_ok(result, 'change beside column-0 procedures passes')
    call write_text(project//'/src/flat.f90', flat_module('2', '      '))
    call git([character(len=64) :: 'add', 'src/flat.f90'])
    call hook()
    call assert_true(result%exit_code /= 0, 'misindented line beside column-0 procedures blocks')
    call assert_contains(result%stdout, 'flat.f90:17: needs formatting', &
        'misindented column-0 neighbour line is reported')

    call remove_tree(scratch)
    call finish_assertions()

contains

    subroutine git(words)
        character(len=*), intent(in) :: words(:)
        integer :: i
        args = string_list_t()
        do i = 1, size(words)
            call list_add(args, trim(words(i)))
        end do
        call run_external('git', args, project, result)
        call assert_process_ok(result, 'git fixture command')
    end subroutine git

    subroutine hook()
        type(string_list_t) :: environment
        args = string_list_t()
        call list_add(args, 'hook')
        call list_add(args, 'pre-commit')
        call list_add(args, '--json')
        call list_add(environment, 'GIT_CONFIG_GLOBAL='//scratch//'/hook-gitconfig')
        call run_fo(driver, args, project, scratch//'/cache', result, &
            extra_environment=environment, timeout_ms=600000)
    end subroutine hook

    function flat_module(step, indent) result(text)
        ! Column-0 procedures, four-space steps and one stray three-space line.
        character(len=*), intent(in) :: step, indent
        character(len=:), allocatable :: text
        integer :: i
        text = 'module flat_mod'//nl//'    implicit none'//nl
        do i = 1, 9
            text = text//'    integer :: a'//achar(iachar('0') + i)//nl
        end do
        text = text//'   integer :: odd'//nl//'contains'//nl// &
            'subroutine bump(x)'//nl//'    integer, intent(inout) :: x'//nl// &
            '    if (x > 0) then'//nl//indent//'x = x + '//step//nl// &
            '    end if'//nl//'end subroutine bump'//nl//'end module flat_mod'//nl
    end function flat_module

    function two_space_module(step, indent) result(text)
        character(len=*), intent(in) :: step, indent
        character(len=:), allocatable :: text
        text = 'module indent_mod'//nl//'  implicit none'//nl//'contains'//nl// &
            '  subroutine bump(x)'//nl//'    integer, intent(inout) :: x'//nl// &
            indent//'x = x + '//step//nl//'  end subroutine bump'//nl// &
            'end module indent_mod'//nl
    end function two_space_module

end program test_hook_format
