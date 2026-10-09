program test_lint_deep_cli
    use, intrinsic :: iso_c_binding, only: c_char, c_int, c_null_char
    use, intrinsic :: iso_fortran_env, only: output_unit
    use fo_test_harness, only: string_list_t, process_result_t, list_add
    use fo_test_harness, only: make_scratch, make_directory, join_path, &
                               current_directory
    use fo_test_harness, only: write_text, append_text, read_text, file_exists
    use fo_test_harness, only: environment_value, make_symlink, remove_path, remove_tree
    use fo_test_harness, only: assert_true, assert_equal_integer, assert_equal_string
    use fo_test_harness, only: assert_contains, assert_not_contains, finish_assertions
    use fo_test_cli, only: resolve_driver, run_fo, run_external
    use fo_test_json, only: json_value_t, json_parse, json_member, json_size, &
                            json_element
    use fo_test_json, only: json_string_value
    use fo_lint, only: lint_warning_t, lint_finding_t, lint_all_json
    use fo_fs, only: fs_copy_exec, fs_find_executable, fs_collect_files, fs_sleep_ms
    use fo_process, only: process_exit
    implicit none

    interface
        integer(c_int) function chmod_path(path, mode) bind(C, name='chmod')
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: path(*)
            integer(c_int), value :: mode
        end function chmod_path
    end interface

    character(:), allocatable :: driver, scratch, project, cache, binary, cwd
    character(:), allocatable :: source, tool, config, message, error, self, native_path
    character(:), allocatable :: session, snapshot
    character(len=4096) :: executable, compiler
    character(len=16) :: argument
    character(len=512) :: blobs(128)
    type(string_list_t) :: arguments, environment, missing_environment
    type(process_result_t) :: result
    type(json_value_t) :: report, diagnostics, entry, field
    integer :: status, before, i, n_blobs
    logical :: valid, found, corrupted

    call get_command_argument(1, argument)
    if (trim(argument) == 'check') then
        call fake_fluff()
        call process_exit(0)
    end if
    if (trim(argument) == '--real-fluff') then
        call real_fluff_boundary()
        call finish_assertions()
        stop
    end if
    call resolve_driver(driver)
    call make_scratch('fo-lint-deep-cli', scratch)
    project = join_path(scratch, 'project')
    cache = join_path(scratch, 'cache')
    binary = join_path(scratch, 'tools')
    source = join_path(project, 'app/oracle.f90')
    config = join_path(project, '.fluff.nml')
    tool = join_path(binary, 'fluff')
    call make_directory(binary)
    call write_text(join_path(project, 'fpm.toml'), &
                    'name="deep_lint_oracle"'//new_line('a'))
    call write_text(source, bad_source())
    call write_text(config, 'configuration A')
    call get_command_argument(0, executable, status=status)
    call current_directory(cwd)
    self = trim(executable)
    if (self(1:1) /= '/') self = join_path(cwd, self)
    call assert_equal_integer(fs_copy_exec(self, tool), 0, 'native fake fluff copies')
    native_path = environment_value('PATH')
    call list_add(environment, 'PATH='//binary//':'//native_path)
    call list_add(environment, 'FO_GREMLIN_STATE_DIR='//join_path(scratch, 'state'))
    call test_native_json()
    call test_cmake_without_manifest()

    call lint(.true., .true.)
    call assert_equal_integer(result%exit_code, 1, &
                              'diagnostic tool exit remains nonzero')
    call assert_equal_integer(run_count() - before, 1, 'selected sources analyzed once')
    call parse_report()
    field = json_member(report, 'unused_imports')
    call assert_true(json_size(field) > 0, &
                     'native diagnostics merge with deep findings')
    diagnostics = json_member(report, 'deep_diagnostics')
    call assert_equal_integer(json_size(diagnostics), 1, &
                              'one deep diagnostic, no duplication')
    entry = json_element(diagnostics, 1)
    field = json_member(entry, 'message')
    message = json_string_value(field)
    call assert_contains(message, 'deep "quoted" café', 'quoted UTF-8 message survives')
    call assert_contains(message, 'END-DEEP', 'large late diagnostic survives')
    call assert_contains(result%stdout, '9223372036854775807', &
                         'extra int64 remains exact')
    field = json_member(entry, 'fixes')
    call assert_equal_integer(json_size(field), 1, &
                              'fix suggestions remain in full object')

    snapshot = result%stdout
    call lint(.true., .true.)
    call assert_equal_integer(run_count() - before, 0, &
                              'unchanged deep result is reused')
    call assert_equal_string(result%stdout, snapshot, 'warm diagnostics are unchanged')
    call append_text(source, '! SOURCE_VARIANT'//new_line('a'))
    call lint(.true., .true.)
    call assert_equal_integer(run_count() - before, 1, &
                              'real source byte edit invalidates cache')
    call assert_contains(result%stdout, 'SOURCE_VARIANT', &
                         'new source changes real diagnostics')
    call write_text(config, 'configuration B')
    call lint(.true., .true.)
    call assert_equal_integer(run_count() - before, 1, &
                              'configuration bytes invalidate reuse')
    call assert_contains(result%stdout, 'configuration B', &
                         'tool reads changed configuration')
    call append_text(tool, 'TOOL_REVISION_2')
    call lint(.true., .true.)
    call assert_equal_integer(run_count() - before, 1, &
                              'tool byte changes invalidate reuse')
    call write_text(join_path(project, 'fluff.nml'), 'secondary configuration')
    call lint(.true., .true.)
    call assert_equal_integer(run_count() - before, 1, &
                              'both discovered config identities count')
    call lint(.true., .false.)
    call assert_equal_integer(run_count() - before, 0, &
                              'human rendering reuses same analysis')
    call assert_contains(result%stdout, 'warning F999', &
                         'human output includes severity and code')
    call assert_contains(result%stdout, 'END-DEEP', &
                         'human output retains late message bytes')

    call lint(.false., .true.)
    call assert_equal_integer(run_count() - before, 0, &
                              'ordinary lint never invokes fluff')
    call assert_not_contains(result%stdout, 'deep_diagnostics', &
                             'ordinary JSON stays native')
    call test_ordinary_commands()
    call test_subdirectory_configuration()

    call write_text(config, 'quiet')
    call write_text(source, good_source())
    call lint(.true., .true.)
    call assert_equal_integer(result%exit_code, 0, 'clean native and deep results pass')
    call assert_equal_integer(run_count() - before, 1, 'clean changed inputs run once')
    call lint(.true., .true.)
    call assert_equal_integer(result%exit_code, 0, 'warm clean result passes')
    call assert_equal_integer(run_count() - before, 0, 'clean warm result is cached')

    do i = 1, 3
        select case (i)
        case (1)
            call write_text(config, 'malformed')
        case (2)
            call write_text(config, 'scalar')
        case (3)
            call write_text(config, 'missing-field')
        end select
        call lint(.true., .true.)
        call assert_equal_integer(result%exit_code, 2, &
                                  'invalid JSON/schema is infrastructure failure')
        call parse_report()
        field = json_member(report, 'deep_error')
        call assert_contains(json_string_value(field), 'malformed', &
                             'malformed output is reported')
        call assert_equal_integer(run_count() - before, 1, 'invalid output is analyzed')
        call lint(.true., .true.)
        call assert_equal_integer(run_count() - before, 1, &
                                  'invalid output is never cached as success')
    end do
    call write_text(config, 'crash')
    call lint(.true., .true.)
    call assert_equal_integer(result%exit_code, 2, &
                              'tool execution failure stays infrastructure error')
    call assert_contains(result%stdout, 'exit 3', &
                         'original tool failure code is reported')

    call make_directory(join_path(scratch, 'missing-tools'))
    call fs_find_executable('gfortran', compiler, found)
    call assert_true(found, 'native compiler is available for missing-tool oracle')
    call make_symlink(trim(compiler), join_path(scratch, 'missing-tools/gfortran'))
    call list_add(missing_environment, 'PATH='//join_path(scratch, 'missing-tools'))
    arguments = string_list_t()
    call list_add(arguments, 'lint')
    call list_add(arguments, '--deep')
    call list_add(arguments, '--json')
    before = run_count()
    call run_fo(driver, arguments, project, cache, result, missing_environment)
    call assert_equal_integer(result%exit_code, 2, 'missing fluff fails explicitly')
    call assert_contains(result%stdout, 'fluff not found', &
                         'missing tool diagnostic is useful')
    call assert_equal_integer(run_count() - before, 0, &
                              'missing tool cannot reuse prior PASS')

    call write_text(config, 'quiet')
    cache = join_path(scratch, 'corrupt-cache')
    call lint(.true., .true.)
    call fs_collect_files(cache//'/store/v2/blobs', '', '', '', blobs, n_blobs)
    corrupted = .false.
    do i = 1, n_blobs
        snapshot = read_text(trim(blobs(i)))
        if (index(snapshot, '{"schema":1,') /= 1) cycle
        status = chmod_path(trim(blobs(i))//c_null_char, 384_c_int)
        call assert_equal_integer(status, 0, &
                                  'owned cached artifact allows deliberate corruption')
        call write_text(trim(blobs(i)), 'corrupt diagnostics bytes')
        corrupted = .true.
        exit
    end do
    call assert_true(corrupted, 'independent oracle corrupts the diagnostics artifact')
    call lint(.true., .true.)
    call assert_equal_integer(result%exit_code, 2, &
                              'corrupt diagnostics cannot manufacture PASS')
    call assert_equal_integer(run_count() - before, 0, &
                              'cache corruption is surfaced explicitly')
    call remove_tree(scratch)
    call finish_assertions()

contains

    subroutine test_cmake_without_manifest()
        character(:), allocatable :: root, test_source, parse_error
        type(string_list_t) :: selected
        type(process_result_t) :: child
        type(json_value_t) :: output, warnings, warning, value
        logical :: parsed

        root = join_path(scratch, 'cmake-project')
        test_source = join_path(root, 'test/test_cannot_fail.f90')
        call write_text(join_path(root, 'CMakeLists.txt'), &
            'cmake_minimum_required(VERSION 3.20)'//new_line('a')// &
            'project(lint_fixture LANGUAGES Fortran)'//new_line('a'))
        call write_text(test_source, &
            'program test_cannot_fail'//new_line('a')// &
            'implicit none'//new_line('a')// &
            'print *, "some tests failed"'//new_line('a')// &
            'end program test_cannot_fail'//new_line('a'))
        call write_text(join_path(root, 'src/tool.f90'), &
            'program tool'//new_line('a')// &
            'implicit none'//new_line('a')// &
            'print *, "done"'//new_line('a')// &
            'end program tool'//new_line('a'))
        call list_add(selected, 'lint')
        call list_add(selected, '--json')
        call list_add(selected, 'test/test_cannot_fail.f90')
        call list_add(selected, 'src/tool.f90')
        call run_fo(driver, selected, root, join_path(scratch, 'cmake-cache'), &
            child, environment, timeout_ms=30000)
        call assert_equal_integer(child%exit_code, 1, &
            'CMake lint reports the deliberately vacuous test')
        call json_parse(child%stdout, output, parsed, parse_error)
        call assert_true(parsed, 'CMake lint returns its JSON diagnostic')
        if (parsed) then
            warnings = json_member(output, 'warnings')
            call assert_equal_integer(json_size(warnings), 1, &
                'CMake lint uses default test/ and ignores the src/ tool')
            warning = json_element(warnings, 1)
            value = json_member(warning, 'message')
            call assert_contains(json_string_value(value), 'cannot fail the build', &
                'CMake lint reaches actual Fortran failure-path analysis')
            value = json_member(warning, 'file')
            call assert_equal_string(json_string_value(value), test_source, &
                'CMake lint identifies the defective test source')
        end if
        call assert_not_contains(child%stderr, &
            'cannot open dependency/project manifest', &
            'CMake lint accepts the absent optional fpm manifest')
    end subroutine test_cmake_without_manifest

    subroutine test_subdirectory_configuration()
        character(:), allocatable :: directory, counter, subconfig

        directory = join_path(project, 'app')
        counter = join_path(directory, '.deep-lint-count')
        subconfig = join_path(directory, '.fluff.nml')
        call write_text(subconfig, 'SUBDIRECTORY_CONFIGURATION_A')
        arguments = string_list_t()
        call list_add(arguments, 'lint')
        call list_add(arguments, '--deep')
        call list_add(arguments, '--json')
        call list_add(arguments, source)
        call run_fo(driver, arguments, directory, cache, result, environment)
        call assert_equal_integer(result%exit_code, 1, 'subdirectory analysis runs')
        call assert_contains(result%stdout, 'SUBDIRECTORY_CONFIGURATION_A', &
                     'deep tool reads configuration in the actual invocation directory')
        call assert_equal_string(read_text(counter), 'run'//new_line('a'), &
                                 'selected relative source reaches one tool invocation')
        call run_fo(driver, arguments, directory, cache, result, environment)
        call assert_equal_string(read_text(counter), 'run'//new_line('a'), &
                       'unchanged subdirectory analysis reuses its complete action key')
        call write_text(subconfig, 'SUBDIRECTORY_CONFIGURATION_B')
        call run_fo(driver, arguments, directory, cache, result, environment)
        call assert_contains(result%stdout, 'SUBDIRECTORY_CONFIGURATION_B', &
                         'changed invocation-directory configuration invalidates reuse')
        call assert_equal_string(read_text(counter), repeat('run'//new_line('a'), 2), &
                                 'changed actual configuration starts one new analysis')
    end subroutine test_subdirectory_configuration

    function bad_source() result(text)
        character(:), allocatable :: text
        text = 'program oracle'//new_line('a')// &
               'use iso_fortran_env, only: real64'//new_line('a')// &
              'implicit none'//new_line('a')//'print *, 1 ! DEEP_BAD'//new_line('a')// &
               'end program oracle'//new_line('a')
    end function bad_source

    function good_source() result(text)
        character(:), allocatable :: text
        text = 'program oracle'//new_line('a')//'implicit none'//new_line('a')// &
               'print *, 1'//new_line('a')//'end program oracle'//new_line('a')
    end function good_source

    subroutine lint(deep, json)
        logical, intent(in) :: deep, json
        arguments = string_list_t()
        call list_add(arguments, 'lint')
        if (deep) call list_add(arguments, '--deep')
        if (json) call list_add(arguments, '--json')
        call list_add(arguments, source)
        call list_add(arguments, source) ! Repeated selection must not duplicate work.
        before = run_count()
        call run_fo(driver, arguments, project, cache, result, environment)
    end subroutine lint

    subroutine parse_report()
        call json_parse(result%stdout, report, valid, error)
        call assert_true(valid, 'public lint report is complete JSON: '//error)
    end subroutine parse_report

    integer function run_count() result(count)
        character(:), allocatable :: text
        integer :: offset
        count = 0
        if (.not. file_exists(join_path(project, '.deep-lint-count'))) return
        text = read_text(join_path(project, '.deep-lint-count'))
        do offset = 1, len(text)
            if (text(offset:offset) == new_line('a')) count = count + 1
        end do
    end function run_count

    subroutine fake_fluff()
        character(len=4096) :: filename, option
        character(:), allocatable :: input, settings, body, suffix
        integer :: unit

        call get_command_argument(2, option)
        if (trim(option) /= '--output-format') call process_exit(3)
        call get_command_argument(3, option)
        if (trim(option) /= 'json') call process_exit(3)
        open (newunit=unit, file='.deep-lint-count', status='unknown', &
              position='append', action='write')
        write (unit, '(a)') 'run'
        close (unit)
        if (command_argument_count() /= 4) call process_exit(3)
        call get_command_argument(4, filename)
        input = read_text(trim(filename))
        settings = read_text('.fluff.nml')
        if (index(settings, 'malformed') > 0) then
            write (output_unit, '(a)') '[{"file":'
            call process_exit(1)
        end if
        if (index(settings, 'scalar') > 0) then
            write (output_unit, '(a)') '[1]'
            call process_exit(1)
        end if
        if (index(settings, 'missing-field') > 0) then
            write (output_unit, '(a)') '[{"file":"oracle.f90"}]'
            call process_exit(1)
        end if
        if (index(settings, 'crash') > 0) then
            write (output_unit, '(a)') '[]'
            call process_exit(3)
        end if
        if (index(settings, 'quiet') > 0 .or. index(input, 'DEEP_BAD') == 0) then
            write (output_unit, '(a)') '[]'
            call process_exit(0)
        end if
        suffix = ''
        if (index(input, 'SOURCE_VARIANT') > 0) suffix = ' SOURCE_VARIANT'
        body = '[{"file":"'//trim(filename)//'","code":"F999",'// &
               '"severity":"warning","message":"deep \"quoted\" café '// &
               repeat('z', 24000)//' END-DEEP '//settings//suffix//'",'// &
               '"location":{"start":{"line":2,"column":1},'// &
               '"end":{"line":2,"column":6}},"revision":9223372036854775807,'// &
               '"fixes":[{"description":"known \"fix\"",'// &
               '"edits":[{"new_text":"replacement"}]}]}]'
        write (output_unit, '(a)') body
        call process_exit(1)
    end subroutine fake_fluff

    subroutine test_native_json()
        type(lint_warning_t) :: warnings(80)
        type(lint_finding_t) :: findings(1)
        character(:), allocatable :: encoded, expected
        type(json_value_t) :: parsed, array, last, value
        integer :: item
        logical :: ok

        expected = repeat('x', 480)//' late "native" café'
        do item = 1, size(warnings)
            warnings(item)%file = 'independent.f90'
            warnings(item)%line = item
            warnings(item)%column = 7
            warnings(item)%message = expected
        end do
        encoded = lint_all_json(findings, 0, warnings, size(warnings))
        call json_parse(encoded, parsed, ok, error)
        call assert_true(ok, 'large native lint report remains valid JSON')
        array = json_member(parsed, 'warnings')
        call assert_equal_integer(json_size(array), 80, 'all native warnings survive')
        last = json_element(array, 80)
        value = json_member(last, 'message')
        call assert_equal_string(json_string_value(value), expected, &
                                'last native diagnostic retains complete escaped bytes')
    end subroutine test_native_json

    subroutine test_ordinary_commands()
        character(:), allocatable :: marker
        integer :: tries

        marker = join_path(scratch, 'ordinary-smoke.done')
        call write_text(join_path(project, 'test/test_smoke.f90'), &
                 'program test_smoke'//new_line('a')//'implicit none'//new_line('a')// &
                        'integer :: u'//new_line('a')// &
               "open(newunit=u,file='"//marker//"',status='replace')"//new_line('a')// &
                   "write(u,'(a)') 'done'"//new_line('a')//'close(u)'//new_line('a')// &
                        'end program test_smoke'//new_line('a'))
        arguments = string_list_t()
        call list_add(arguments, 'build')
        before = run_count()
        call run_fo(driver, arguments, project, cache, result, environment)
        call assert_equal_integer(result%exit_code, 0, 'ordinary build runs')
        call assert_equal_integer(run_count() - before, 0, 'build never starts fluff')
        arguments = string_list_t()
        call list_add(arguments, 'test')
        call list_add(arguments, '--all')
        call list_add(arguments, 'test_smoke')
        before = run_count()
        call run_fo(driver, arguments, project, cache, result, environment)
        call assert_equal_integer(result%exit_code, 0, 'ordinary test runs')
        call assert_equal_integer(run_count() - before, 0, 'test never starts fluff')
        call remove_path(marker)
        arguments = string_list_t()
        call list_add(arguments, 'gremlin')
        call list_add(arguments, 'start')
        call list_add(arguments, '--lane')
        call list_add(arguments, 'deep-no-implicit')
        call list_add(arguments, '--random')
        call list_add(arguments, '0')
        call list_add(arguments, '--target')
        call list_add(arguments, 'test_smoke')
        before = run_count()
        call run_fo(driver, arguments, project, cache, result, environment)
     call assert_equal_integer(result%exit_code, 0, 'resident ordinary campaign starts')
        do tries = 1, 600
            if (file_exists(marker)) exit
            call fs_sleep_ms(50)
        end do
        call assert_true(file_exists(marker), 'resident native test actually executes')
        arguments = string_list_t()
        call list_add(arguments, 'gremlin')
        call list_add(arguments, 'stop')
        call list_add(arguments, '--lane')
        call list_add(arguments, 'deep-no-implicit')
        call run_fo(driver, arguments, project, cache, result, environment)
        call assert_equal_integer(result%exit_code, 0, 'resident oracle owner stops')
        call assert_equal_integer(run_count() - before, 0, 'Gremlin never starts fluff')
    end subroutine test_ordinary_commands

    subroutine real_fluff_boundary()
        character(len=4096) :: actual_tool
        character(len=24) :: name
        character(:), allocatable :: root, filename, long_file, parse_error
        type(string_list_t) :: args
        type(process_result_t) :: child
        type(json_value_t) :: array, diagnostic, value
        logical :: ok
        integer :: item

        call get_command_argument(2, actual_tool)
       call assert_true(file_exists(trim(actual_tool)), 'exact real fluff image exists')
        call make_scratch('fo-real-fluff-boundary', root)
        args = string_list_t()
        call list_add(args, 'check')
        call list_add(args, '--output-format')
        call list_add(args, 'json')
        call list_add(args, '--select')
        call list_add(args, 'F001')
        do item = 1, 120
            write (name, '("clean_",i0,".f90")') item
            filename = join_path(root, trim(name))
            call write_text(filename, good_source())
            call list_add(args, filename)
        end do
        long_file = root//'/'//repeat('a', 140)//'/'//repeat('b', 140)// &
                    '/late_bad.f90'
        call write_text(long_file, 'program late_bad'//new_line('a')// &
                     'print *, 1'//new_line('a')//'end program late_bad'//new_line('a'))
        call list_add(args, long_file)
        call run_external(trim(actual_tool), args, root, child, timeout_ms=180000)
        call assert_equal_integer(child%exit_code, 1, &
                                  'real fluff diagnoses late selected file')
        call json_parse(child%stdout, array, ok, parse_error)
        call assert_true(ok, 'real fluff emits complete diagnostic JSON: '//parse_error)
        call assert_equal_integer(json_size(array), 1, &
              'more than 100 actual files retain only the independently bad diagnostic')
        diagnostic = json_element(array, 1)
        value = json_member(diagnostic, 'file')
        call assert_equal_string(json_string_value(value), long_file, &
                           'long public CLI argument reaches linter without truncation')
        value = json_member(diagnostic, 'code')
        call assert_equal_string(json_string_value(value), 'F001', &
                             'real rule detects missing implicit none in the late file')
        call remove_tree(root)
    end subroutine real_fluff_boundary

end program test_lint_deep_cli
