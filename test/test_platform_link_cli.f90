program test_platform_link_cli
    use fo_test_harness, only: string_list_t, process_result_t, list_add
    use fo_test_harness, only: make_scratch, join_path, write_text, write_lines, remove_tree
    use fo_test_harness, only: assert_true, assert_equal_string, assert_equal_integer
    use fo_test_harness, only: assert_contains, assert_process_ok
    use fo_test_cli, only: resolve_driver, run_fo, run_external, parse_json_report
    use fo_test_json, only: json_value_t, json_member, json_element, json_size
    use fo_test_json, only: json_string_value
    implicit none

    character(:), allocatable :: driver, scratch, dependency, consumer, external
    character(:), allocatable :: mode, manifest, library_dir, marker, platform
    type(string_list_t) :: arguments, environment
    type(process_result_t) :: result, tool_result
    type(json_value_t) :: report, tests, entry, field
    integer :: mode_index, warm

    call resolve_driver(driver)
    call make_scratch('fo-platform-link', scratch)
    dependency = join_path(scratch, 'dependency')
    consumer = join_path(scratch, 'consumer')
    external = join_path(scratch, 'external')
    call write_text(join_path(dependency, 'fpm.toml'), 'name = "depstate"' // new_line('a'))
    call write_lines(join_path(dependency, 'src/dep_state.f90'), &
        [character(len=96) :: 'module dep_state', 'implicit none', 'integer :: stored = 7', &
        'end module dep_state'])
    call initialize_dependency_git(dependency)
    call write_text(join_path(external, 'probe.c'), 'int probe_value(void) { return 42; }' // new_line('a'))
    call run_tool('cc', words([character(len=64) :: '-fPIC', '-c', 'probe.c', '-o', 'probe.o']), &
        external)
    call run_tool('ar', words([character(len=64) :: 'rcs', 'libplatform_probe.a', 'probe.o']), &
        external)

    call write_lines(join_path(consumer, 'src/provider.f90'), [character(len=128) :: &
        'module provider', 'use dep_state, only: stored', &
        'use, intrinsic :: iso_c_binding, only: c_int', 'implicit none', 'interface', &
        'integer(c_int) function probe_value() bind(c)', 'import c_int', &
        'end function probe_value', 'end interface', 'contains', 'subroutine update()', &
        'stored = probe_value()', 'end subroutine update', 'end module provider'])
    call write_link_program(join_path(consumer, 'app/probe.f90'), 'probe')
    call write_link_program(join_path(consumer, 'test/test_probe.f90'), 'test_probe')
    call list_add(environment, 'LIBRARY_PATH=' // external)

    do mode_index = 1, 2
        if (mode_index == 1) then
            mode = 'static'
        else
            mode = 'shared'
        end if
        manifest = 'name = "platform_link_probe"' // new_line('a') // '[dependencies]' // &
            new_line('a') // 'depstate = { git = "file://' // dependency // &
            '", branch = "main" }' // new_line('a') // '[build]' // new_line('a') // &
            'link = ["platform_probe"]' // new_line('a') // '[extra.fo]' // new_line('a') // &
            'link = "' // mode // '"' // new_line('a') // 'pic = "true"' // new_line('a')
        call write_text(join_path(consumer, 'fpm.toml'), manifest)
        do warm = 1, 2
            arguments = words([character(len=32) :: 'exec', 'probe'])
            call run_fo(driver, arguments, consumer, join_path(scratch, '.fo-cache'), &
                result, environment)
            call assert_process_ok(result, mode // ' app links Git and static archive dependencies')
            call assert_equal_string(result%stdout, '42' // new_line('a'), &
                mode // ' app resolves native archive symbol')
            arguments = words([character(len=32) :: 'test', '--all', '--json'])
            call run_fo(driver, arguments, consumer, join_path(scratch, '.fo-cache'), &
                result, environment)
            call assert_process_ok(result, mode // ' test links dependencies')
            call parse_json_report(result, report, mode // ' JSON test report')
            call check_single_test(report, mode // ' test report')
        end do
    end do

    call run_external('uname', words([character(len=16) :: '-s']), consumer, tool_result)
    call assert_process_ok(tool_result, 'read host operating system')
    platform = trim(tool_result%stdout)
    if (platform == 'Darwin' // new_line('a')) call exercise_macos_profiles()

    call remove_tree(scratch)
    write(*, '(a)') 'platform-link-cli: static/shared cold/warm dependency and archive links pass'

contains

    function words(items) result(list)
        character(len=*), intent(in) :: items(:)
        type(string_list_t) :: list
        integer :: i

        do i = 1, size(items)
            call list_add(list, trim(items(i)))
        end do
    end function words

    subroutine run_tool(executable, command, directory)
        character(len=*), intent(in) :: executable, directory
        type(string_list_t), intent(in) :: command
        type(process_result_t) :: child

        call run_external(executable, command, directory, child)
        call assert_process_ok(child, 'fixture tool: ' // executable)
    end subroutine run_tool

    subroutine initialize_dependency_git(directory)
        character(len=*), intent(in) :: directory
        type(string_list_t) :: command

        command = words([character(len=64) :: 'init', '--quiet', '--initial-branch=main'])
        call run_tool('git', command, directory)
        command = words([character(len=64) :: 'add', 'fpm.toml', 'src/dep_state.f90'])
        call run_tool('git', command, directory)
        command = words([character(len=96) :: '-c', 'user.name=Fixture', '-c', &
            'user.email=fixture@example.invalid', '-c', 'commit.gpgsign=false', &
            'commit', '--quiet', '-m', 'Dependency fixture'])
        call run_tool('git', command, directory)
    end subroutine initialize_dependency_git

    subroutine write_link_program(path, name)
        character(len=*), intent(in) :: path, name

        call write_text(path, 'program ' // trim(name) // new_line('a') // &
            'use dep_state, only: stored' // new_line('a') // &
            'use provider, only: update' // new_line('a') // 'implicit none' // &
            new_line('a') // 'call update()' // new_line('a') // &
            'if (stored /= 42) stop 1' // new_line('a') // &
            "print '(i0)', stored" // new_line('a') // &
            'end program ' // trim(name) // new_line('a'))
    end subroutine write_link_program

    subroutine check_single_test(document, context)
        type(json_value_t), intent(in) :: document
        character(len=*), intent(in) :: context
        type(json_value_t) :: entries, item, field

        entries = json_member(document, 'tests')
        call assert_equal_integer(json_size(entries), 1, context // ': one test result')
        item = json_element(entries, 1)
        field = json_member(item, 'name')
        call assert_equal_string(json_string_value(field), 'test_probe', context // ': test name')
        field = json_member(item, 'status')
        call assert_equal_string(json_string_value(field), 'pass', context // ': test passes')
    end subroutine check_single_test

    subroutine exercise_macos_profiles()
        type(string_list_t) :: command, listing_environment
        type(process_result_t) :: child, listing, otool
        type(json_value_t) :: document
        integer :: profile_index, file_index
        character(:), allocatable :: profile, image_one, image_two, file_name, full_path

        library_dir = join_path(consumer, 'build/fo/lib')
        call list_add(listing_environment, 'LC_ALL=C')
        image_one = ''
        image_two = ''
        do profile_index = 1, 2
            if (profile_index == 1) then
                profile = 'profile-one'
                marker = join_path(scratch, profile)
            else
                profile = 'profile-two'
                marker = join_path(scratch, profile)
            end if
            command = words([character(len=32) :: 'test', '--all', '--json', '--flag'])
            call list_add(command, '-Wl,-rpath,' // marker)
            call run_fo(driver, command, consumer, join_path(scratch, '.fo-cache'), child, environment)
            call assert_process_ok(child, 'macOS shared image test for ' // profile)
            call parse_json_report(child, document, 'macOS profile report')
            call check_single_test(document, 'macOS profile report')
            call run_external('/bin/ls', words([character(len=128) :: '-1', library_dir]), &
                consumer, listing, listing_environment)
            call assert_process_ok(listing, 'list shared library images')
            file_index = 1
            do
                file_name = line_at(listing%stdout, file_index)
                if (len(file_name) == 0) exit
                file_index = file_index + 1
                if (len(file_name) < 6) cycle
                if (file_name(len(file_name) - 5:) /= '.dylib') cycle
                full_path = join_path(library_dir, file_name)
                call run_external('otool', words([character(len=256) :: '-l', full_path]), &
                    consumer, otool)
                call assert_process_ok(otool, 'inspect shared image rpath')
                if (index(otool%stdout, 'path ' // marker // ' (') == 0) cycle
                if (profile_index == 1) then
                    image_one = file_name
                else
                    image_two = file_name
                end if
                exit
            end do
            call assert_true(len(image_one) > 0 .or. profile_index == 2, &
                'first native image retains its requested rpath')
            if (profile_index == 2) call assert_true(len(image_two) > 0, &
                'second native image retains its requested rpath')
        end do
        call assert_true(image_one /= image_two, 'flag profiles retain separate native images')
    end subroutine exercise_macos_profiles

    function line_at(text, number) result(line)
        character(len=*), intent(in) :: text
        integer, intent(in) :: number
        character(:), allocatable :: line
        integer :: i, current, start

        current = 1
        start = 1
        do i = 1, len(text)
            if (text(i:i) /= new_line('a')) cycle
            if (current == number) then
                line = text(start:i - 1)
                return
            end if
            current = current + 1
            start = i + 1
        end do
        if (current == number .and. start <= len(text)) then
            line = text(start:)
        else
            line = ''
        end if
    end function line_at

end program test_platform_link_cli
