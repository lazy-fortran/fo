program test_exec_app_cli
    use fo_test_harness, only: string_list_t, process_result_t, list_add
    use fo_test_harness, only: make_scratch, join_path, write_text, remove_tree
    use fo_test_harness, only: assert_true, assert_equal_string, assert_equal_integer
    use fo_test_harness, only: assert_contains, assert_not_contains, assert_process_ok
    use fo_test_cli, only: resolve_driver, run_fo
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

    call exercise_flavours(flavours)
    call exercise_format_check(unformatted)
    call setup_project('broken_test', 'name = "broken_test"' // new_line('a'), &
        'test/healthy.f90', .true., broken)
    call write_program(join_path(broken, 'test/healthy.f90'), 'healthy')
    call invoke(broken, words([character(len=32) :: 'test', 'broken']), result)
    call assert_equal_integer(result%exit_code, 1, 'explicitly requested broken test fails compile')
    call assert_equal_integer(result%term_signal, 0, 'broken test reports status, not a signal')

    call remove_tree(scratch)
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
