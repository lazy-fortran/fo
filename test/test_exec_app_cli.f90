program test_exec_app_cli
    use fo_test_harness, only: string_list_t, process_result_t, list_add
    use fo_test_harness, only: make_scratch, join_path, write_text, remove_tree
    use fo_test_harness, only: assert_true, assert_equal_string, assert_equal_integer
    use fo_test_harness, only: assert_contains, assert_not_contains, assert_process_ok
    use fo_test_harness, only: file_exists
    use fo_test_cli, only: resolve_driver, run_fo, run_external
    use fo_test_harness, only: finish_assertions
    implicit none

    character(:), allocatable :: driver, scratch, broken, flavours, unformatted
    type(process_result_t) :: result
    type(string_list_t) :: arguments

    call resolve_driver(driver)
    call make_scratch('fo-exec-app', scratch)

    call exercise_fixture('package_main', 'name = "test_package"' // new_line('a'), &
        'app/main.f90', 'test_package', .true.)
    call exercise_fixture('discovered_app', 'name = "discovery"' // new_line('a'), &
        'app/test_auto.f90', 'test_auto', .true.)
    call exercise_fixture('explicit_app', &
        'name = "explicit_app"' // new_line('a') // '[build]' // new_line('a') // &
        'auto-executables = false' // new_line('a') // '[[executable]]' // new_line('a') // &
        'name = "test_public_app"' // new_line('a') // 'source-dir = "app"' // &
        new_line('a') // 'main = "launcher.f90"' // new_line('a'), &
        'app/launcher.f90', 'test_public_app', .true.)
    call exercise_fixture('example_main', 'name = "different_package"' // new_line('a'), &
        'example/main.f90', 'main', .true.)
    call exercise_fixture('nested_example', 'name = "nested_package"' // new_line('a'), &
        'example/demo/demo.f90', 'demo', .true.)
    call exercise_fixture('explicit_test', &
        'name = "explicit_test"' // new_line('a') // '[[test]]' // new_line('a') // &
        'name = "smoke"' // new_line('a') // 'source-dir = "test"' // new_line('a') // &
        'main = "test_check.f90"' // new_line('a'), &
        'test/test_check.f90', 'smoke', .false.)

    call setup_project('explicit_collision', &
        'name = "collision"' // new_line('a') // '[[executable]]' // new_line('a') // &
        'name = "shared"' // new_line('a') // 'source-dir = "app"' // new_line('a') // &
        'main = "launcher.f90"' // new_line('a') // '[[test]]' // new_line('a') // &
        'name = "shared"' // new_line('a') // 'source-dir = "test"' // new_line('a') // &
        'main = "check.f90"' // new_line('a'), 'test/check.f90', .false., broken)
    call write_program(join_path(broken, 'app/launcher.f90'), 'application')
    call exercise_source(broken, 'test/check.f90', 'shared')

    call setup_project('discovered_collision', 'name = "collision"' // new_line('a'), &
        'test/shared.f90', .false., broken)
    call write_program(join_path(broken, 'app/shared.f90'), 'application')
    call exercise_source(broken, 'test/shared.f90', 'shared')

    call exercise_custom_targets()
    call exercise_flavours(flavours)
    call exercise_format_check(unformatted)
    call setup_project('broken_test', 'name = "broken_test"' // new_line('a'), &
        'test/healthy.f90', .true., broken)
    call write_program(join_path(broken, 'test/healthy.f90'), 'healthy')
    call invoke(broken, words([character(len=32) :: 'test', 'broken']), result)
    call assert_equal_integer(result%exit_code, 1, 'explicitly requested broken test fails compile')
    call assert_equal_integer(result%term_signal, 0, 'broken test reports status, not a signal')

    call remove_tree(scratch)
    call finish_assertions()
    write(*, '(a)') 'exec-app-cli: target kinds, cache updates, flags, argv and diagnostics pass'

contains

    function words(items) result(list)
        character(len=*), intent(in) :: items(:)
        type(string_list_t) :: list
        integer :: i

        do i = 1, size(items)
            call list_add(list, trim(items(i)))
        end do
    end function words

    subroutine setup_project(name, manifest, source, poison, project)
        character(len=*), intent(in) :: name, manifest, source
        logical, intent(in) :: poison
        character(:), allocatable, intent(out) :: project

        project = join_path(scratch, name)
        call write_text(join_path(project, 'fpm.toml'), manifest)
        if (poison) call write_text(join_path(project, 'test/broken.f90'), &
            'program broken' // new_line('a') // 'implicit none' // new_line('a') // &
            'print *, missing_symbol' // new_line('a') // 'end program broken' // new_line('a'))
        call write_program(join_path(project, source), 'initial')
    end subroutine setup_project

    subroutine exercise_fixture(name, manifest, source, target, poison)
        character(len=*), intent(in) :: name, manifest, source, target
        logical, intent(in) :: poison
        character(:), allocatable :: project

        call setup_project(name, manifest, source, poison, project)
        call exercise_source(project, source, target)
    end subroutine exercise_fixture

    subroutine exercise_source(project, source, target)
        character(len=*), intent(in) :: project, source, target
        character(len=32), parameter :: values(3) = [character(len=32) :: &
            'first', 'first', 'updated_longer_value']
        type(process_result_t) :: child
        integer :: i
        character(len=32) :: previous
        type(string_list_t) :: command

        previous = ''
        do i = 1, size(values)
            if (trim(values(i)) /= trim(previous)) then
                call write_program(join_path(project, source), trim(values(i)))
                previous = values(i)
            end if
            command = words([character(len=32) :: 'exec', trim(target)])
            call invoke(project, command, child)
            call assert_process_ok(child, trim(target) // ' cold/warm/updated execution')
            call assert_equal_string(child%stdout, trim(values(i)) // new_line('a'), &
                trim(target) // ' executes the current source')
        end do
    end subroutine exercise_source

    subroutine write_program(path, value)
        character(len=*), intent(in) :: path, value

        call write_text(path, 'program internal_name' // new_line('a') // &
            'implicit none' // new_line('a') // "print '(a)', '" // trim(value) // "'" // &
            new_line('a') // 'end program internal_name' // new_line('a'))
    end subroutine write_program

    subroutine invoke(project, command, child)
        character(len=*), intent(in) :: project
        type(string_list_t), intent(in) :: command
        type(process_result_t), intent(out) :: child

        call run_fo(driver, command, project, join_path(scratch, '.fo-cache'), child)
    end subroutine invoke

    subroutine exercise_custom_targets()
        character(:), allocatable :: project, prefix
        character(len=1024) :: reference
        character(len=1), parameter :: nl = new_line('a')
        character(len=2), parameter :: expected(3) = ['61', '61', '63']
        type(string_list_t) :: command
        type(process_result_t) :: child
        integer :: phase, status

        project = join_path(scratch, 'custom-targets')
        prefix = join_path(scratch, 'custom-prefix')
        reference = ''
        call get_environment_variable('FO_FPM_REFERENCE', reference, status=status)
        if (status /= 0) reference = ''
        call write_text(project//'/fpm.toml', &
            'name = "custom_targets"'//nl//'[build]'//nl// &
            'auto-executables = false'//nl//'auto-examples = false'//nl// &
            '[[executable]]'//nl//'name = "test_report"'//nl// &
            'source-dir = "commands/report"'//nl//'main = "main.f90"'//nl// &
            '[[executable]]'//nl//'name = "other_report"'//nl// &
            'source-dir = "commands/other"'//nl//'main = "main.f90"'//nl)
        call write_text(project//'/src/base.f90', &
            'module base'//nl//'implicit none'//nl//'contains'//nl// &
            'integer function base_value()'//nl//'base_value = 43'//nl// &
            'end function'//nl//'end module'//nl)
        call write_text(project//'/commands/report/main.f90', &
            'program report'//nl//'use report_math, only: report_value'//nl// &
            'implicit none'//nl//"print '(a,i0)', 'CUSTOM_OUTPUT=', report_value()"// &
            nl//'end program'//nl)
        call write_text(project//'/commands/other/main.f90', &
            'program other'//nl//'use base, only: base_value'//nl// &
            'implicit none'//nl//"print '(a,i0)', 'OTHER_OUTPUT=', base_value()+24"// &
            nl//'end program'//nl)
        call write_program(project//'/commands/report/unregistered.f90', 'unused')
        call write_text(project//'/test/broken.f90', &
            'program broken'//nl//'implicit none'//nl// &
            'print *, missing_symbol'//nl//'end program'//nl)
        call write_report_math(project, '18')
        do phase = 1, size(expected)
            if (phase == 3) call write_report_math(project, '20')
            if (len_trim(reference) > 0) then
                command = words([character(len=32) :: 'run', 'test_report'])
                call run_external(trim(reference), command, project, child)
                call assert_process_ok(child, 'pinned FPM custom executable runs')
                call assert_contains(child%stdout, 'CUSTOM_OUTPUT='//expected(phase), &
                    'pinned FPM helper body controls cold/warm/edited runtime')
            end if
            command = words([character(len=32) :: 'exec', 'test_report'])
            call invoke(project, command, child)
            call assert_process_ok(child, 'custom executable avoids unrelated tests')
            call assert_equal_string(child%stdout, &
                'CUSTOM_OUTPUT='//expected(phase)//nl, &
                'declared custom executable follows cold/warm/helper-body edits')
        end do
        command = words([character(len=32) :: 'run', 'other_report'])
        call invoke(project, command, child)
        call assert_process_ok(child, 'second custom directory runs its public name')
        call assert_equal_string(child%stdout, 'OTHER_OUTPUT=67'//nl, &
            'same main filename in another directory selects the correct program')
        command = words([character(len=32) :: 'exec', '--no-build', 'unregistered'])
        call invoke(project, command, child)
        call assert_true(child%exit_code /= 0, &
            'disabled automatic discovery excludes another custom-directory main')
        command = words([character(len=32) :: 'install', '--prefix'])
        call list_add(command, prefix)
        call invoke(project, command, child)
        call assert_process_ok(child, 'custom executables install through native build')
        call check_installed(project, prefix, 'test_report', 'CUSTOM_OUTPUT=63')
        call check_installed(project, prefix, 'other_report', 'OTHER_OUTPUT=67')
        if (len_trim(reference) > 0) then
            prefix = join_path(scratch, 'custom-reference-prefix')
            command = words([character(len=32) :: 'install', '--prefix'])
            call list_add(command, prefix)
            call run_external(trim(reference), command, project, child)
            call assert_process_ok(child, 'pinned FPM installs the unchanged project')
            call check_installed(project, prefix, 'test_report', 'CUSTOM_OUTPUT=63')
            call check_installed(project, prefix, 'other_report', 'OTHER_OUTPUT=67')
        end if
    end subroutine exercise_custom_targets

    subroutine write_report_math(project, offset)
        character(len=*), intent(in) :: project, offset
        character(len=1), parameter :: nl = new_line('a')
        call write_text(project//'/commands/report/helper.f90', &
            'module report_math'//nl//'use base, only: base_value'//nl// &
            'implicit none'//nl//'contains'//nl//'integer function report_value()'//nl// &
            'report_value = base_value()+'//offset//nl//'end function'//nl// &
            'end module'//nl)
    end subroutine write_report_math

    subroutine check_installed(project, prefix, name, expected)
        character(len=*), intent(in) :: project, prefix, name, expected
        character(:), allocatable :: installed
        type(process_result_t) :: child
        installed = prefix//'/bin/'//name
        if (.not. file_exists(installed)) installed = installed//'.exe'
        call run_external(installed, string_list_t(), project, child)
        call assert_process_ok(child, 'installed custom executable runs')
        call assert_equal_string(child%stdout, expected//new_line('a'), &
            'installed executable preserves edited runtime and public name')
    end subroutine check_installed

    subroutine exercise_flavours(project)
        character(:), allocatable, intent(out) :: project
        type(string_list_t) :: command
        type(process_result_t) :: child

        project = join_path(scratch, 'flavours')
        call write_text(join_path(project, 'fpm.toml'), 'name = "flavour"' // new_line('a'))
        call write_text(join_path(project, 'app/main.F90'), &
            'program flavour' // new_line('a') // 'implicit none' // new_line('a') // &
            'character(len=64) :: word' // new_line('a') // '#ifdef FAST' // new_line('a') // &
            "print '(a)', 'fast'" // new_line('a') // '#else' // new_line('a') // &
            "print '(a)', 'plain'" // new_line('a') // '#endif' // new_line('a') // &
            'if (command_argument_count() > 0) then' // new_line('a') // &
            'call get_command_argument(1, word)' // new_line('a') // &
            "print '(a)', trim(word)" // new_line('a') // 'end if' // new_line('a') // &
            'end program flavour' // new_line('a'))
        command = words([character(len=32) :: 'build', '--flag', '-DFAST'])
        call invoke(project, command, child)
        call assert_process_ok(child, 'build preprocessor flag profile')
        command = words([character(len=32) :: 'exec', '--no-build', '--flag', &
            '-DFAST', 'flavour'])
        call invoke(project, command, child)
        call assert_process_ok(child, 'execute flag-specific profile')
        call assert_equal_string(child%stdout, 'fast' // new_line('a'), 'FAST build selected')
        command = words([character(len=32) :: 'exec', '--no-build', 'flavour'])
        call invoke(project, command, child)
        call assert_true(child%exit_code /= 0, 'missing default profile fails')
        call assert_contains(child%stderr, "--flag '-DFAST'", 'missing profile names its flags')
        command = words([character(len=32) :: 'build'])
        call invoke(project, command, child)
        call assert_process_ok(child, 'build default profile')
        command = words([character(len=32) :: 'exec', '--no-build', '--flag', &
            '-DFAST', 'flavour'])
        call invoke(project, command, child)
        call assert_process_ok(child, 'FAST profile remains available')
        call assert_equal_string(child%stdout, 'fast' // new_line('a'), 'default keeps FAST profile')
        command = words([character(len=32) :: 'exec', '--no-build', 'flavour'])
        call invoke(project, command, child)
        call assert_process_ok(child, 'execute default profile')
        call assert_equal_string(child%stdout, 'plain' // new_line('a'), 'plain build selected')
        call assert_contains(child%stderr, 'also built with', 'unused profile is reported')

        command = words([character(len=32) :: 'exec', 'flavour', '--flag'])
        call invoke(project, command, child)
        call assert_process_ok(child, 'program receives argument after target')
        call assert_equal_string(child%stdout, 'plain' // new_line('a') // &
            '--flag' // new_line('a'), 'post-target --flag reaches the application')
        command = words([character(len=32) :: 'exec', 'flavour', '--help'])
        call invoke(project, command, child)
        call assert_process_ok(child, 'program receives help argument after target')
        call assert_equal_string(child%stdout, 'plain' // new_line('a') // &
            '--help' // new_line('a'), 'post-target --help reaches the application')
    end subroutine exercise_flavours

    subroutine exercise_format_check(project)
        character(:), allocatable, intent(out) :: project
        type(string_list_t) :: command
        type(process_result_t) :: child

        project = join_path(scratch, 'unformatted')
        call write_text(join_path(project, 'x.f90'), &
            'module m' // new_line('a') // '    implicit none' // new_line('a') // &
            'contains' // new_line('a') // '    function f(x) &' // new_line('a') // &
            '        result(y)' // new_line('a') // '        real, intent(in) :: x' // &
            new_line('a') // '        real :: y' // new_line('a') // '        y = x' // &
            new_line('a') // '    end function f' // new_line('a') // 'end module m' // &
            new_line('a'))
        command = words([character(len=32) :: 'fmt', '--check', 'x.f90'])
        call invoke(project, command, child)
        call assert_equal_integer(child%exit_code, 1, 'fmt --check identifies formatting differences')
        call assert_contains(child%stderr, 'x.f90:5: needs formatting', 'fmt names file and line')
        call assert_not_contains(child%stderr, 'Backtrace', 'fmt failure omits runtime backtrace')
        call assert_not_contains(child%stderr, 'ERROR STOP', 'fmt failure omits Fortran stop text')
    end subroutine exercise_format_check

end program test_exec_app_cli
