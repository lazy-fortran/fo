program test_dispatcher_cli
    use fo_test_harness, only: process_result_t, string_list_t, list_add, list_extend
    use fo_test_harness, only: make_scratch, join_path, current_directory
    use fo_test_harness, only: write_lines, write_text, append_text, move_path
    use fo_test_harness, only: remove_path, remove_tree, assert_equal_integer
    use fo_test_harness, only: assert_equal_string, assert_file_exists, assert_file_absent
    use fo_test_harness, only: assert_file_equals, assert_contains, assert_not_contains
    use fo_test_harness, only: assert_true, assert_process_ok, file_exists, read_text
    use fo_test_cli, only: resolve_driver, run_fo, run_external, parse_json_report
    use fo_test_json, only: json_value_t, json_member, json_element, json_size
    use fo_test_json, only: json_string_value, json_number_value
    implicit none

    character(len=32), parameter :: all_cases(5) = [character(len=32) :: &
        'test_alpha', 'test_beta', 'test_legacy', 'test_plain', 'test_probe_slow']
    character(len=32), parameter :: initial_names(4) = [character(len=32) :: &
        'test_alpha', 'test_beta', 'test_legacy', 'test_plain']
    character(len=64), parameter :: initial_values(4) = [character(len=64) :: &
        'alpha-original', 'beta-original', 'legacy-dispatched', 'plain-original']
    character(len=64), parameter :: changed_values(4) = [character(len=64) :: &
        'alpha-updated', 'beta-original', 'legacy-dispatched', 'plain-original']
    character(len=32), parameter :: none_names(0) = [character(len=32) ::]
    character(len=64), parameter :: none_values(0) = [character(len=64) ::]
    character(len=32), parameter :: plain_name(1) = [character(len=32) :: 'test_plain']
    character(len=64), parameter :: plain_value(1) = [character(len=64) :: 'plain-original']

    character(:), allocatable :: driver, project, scratch, manifest, binary_listing
    character(:), allocatable :: marker_manifest, rewritten
    type(process_result_t) :: result
    type(string_list_t) :: arguments, regex_environment, listing_args, listing_env
    type(json_value_t) :: report, tests, entry, field

    call resolve_driver(driver)
    call current_directory(project)
    call make_scratch('fo-dispatcher-cli', scratch)
    call write_lines(join_path(scratch, 'fpm.toml'), [character(len=128) :: &
        'name = "dispatcher_probe"', '[extra.fo]', 'dispatcher = "test_dispatcher"', &
        'link = "shared"', 'pic = "true"', '[[test]]', 'name = "test_dispatcher"', &
        'source-dir = "test"', 'main = "suite_entry.f90"', '[[test]]', &
        'name = "test_beta"', 'source-dir = "test"', 'main = "nested/beta_source.f90"', &
        '[[test]]', 'name = "test_module_only"', 'source-dir = "test"', &
        'main = "support.f90"'])
    call write_text(join_path(scratch, 'test/test_alpha.f90'), &
        case_source('test_alpha', 'alpha-original', .true.))
    call write_text(join_path(scratch, 'test/nested/beta_source.f90'), &
        case_source('test_beta', 'beta-original', .true.))
    call write_text(join_path(scratch, 'test/support.f90'), &
        case_source('test_legacy', 'legacy-dispatched', .false.))
    call write_lines(join_path(scratch, 'test/test_flat_module.f90'), &
        [character(len=64) :: 'module test_flat_module', 'end module'])
    call write_lines(join_path(scratch, 'test/test_flat_source.f90'), &
        [character(len=64) :: 'module flat_helper', 'end module'])
    call write_lines(join_path(scratch, 'test/test_legacy.f90'), [character(len=64) :: &
        '! fo: dispatcher', 'program test_legacy', 'implicit none', 'stop 89', &
        'end program test_legacy'])
    call write_lines(join_path(scratch, 'test/suite_entry.f90'), [character(len=128) :: &
        'program suite_entry', 'use test_alpha_case, only: alpha => run_case', &
        'use test_beta_case, only: beta => run_case', &
        'use test_legacy_case, only: legacy => run_case', 'implicit none', &
        'character(len=128) :: name', 'if(command_argument_count() /= 1) stop 3', &
        'call get_command_argument(1, name)', 'select case(trim(name))', &
        "case('test_alpha')", 'call alpha()', "case('test_beta')", 'call beta()', &
        "case('test_legacy')", 'call legacy()', 'case default', 'stop 4', &
        'end select', 'end program suite_entry'])
    call write_lines(join_path(scratch, 'test/test_plain.f90'), [character(len=128) :: &
        'program test_plain', 'implicit none', 'integer :: unit', &
        "open(newunit=unit, file='test_plain.receipt', status='replace')", &
        "write(unit, '(a)') 'plain-original'", 'close(unit)', 'end program test_plain'])

    call expect_cases(words([character(len=32) :: 'test', '--all']), initial_names, initial_names, initial_values)
    call expect_cases(words([character(len=32) :: 'test', '--all']), initial_names, initial_names, initial_values)
    call expect_cases(words([character(len=32) :: 'test', '--random', '4', '--seed', '42']), &
        initial_names, initial_names, initial_values)
    call expect_cases(words([character(len=32) :: 'test', '--only-changed']), initial_names, initial_names, initial_values)
    call expect_cases(words([character(len=32) :: 'test', 'test_alpha']), &
        [character(len=32) :: 'test_alpha'], [character(len=32) :: 'test_alpha'], &
        [character(len=64) :: 'alpha-original'])
    call expect_cases(words([character(len=32) :: 'test', 'test_beta']), &
        [character(len=32) :: 'test_beta'], [character(len=32) :: 'test_beta'], &
        [character(len=64) :: 'beta-original'])
    call expect_rejected('test_module_only')
    call expect_rejected('test_flat_module')
    call expect_rejected('test_flat_source')
    call expect_rejected('test_does_not_exist')

    arguments = words([character(len=32) :: 'test', 'test_dispatcher', '--json'])
    call invoke_fo(arguments, result)
    call assert_true(result%exit_code /= 0, 'explicit dispatcher has no implicit self argument')
    call parse_json_report(result, report, 'explicit dispatcher report')
    tests = json_member(report, 'tests')
    call assert_equal_integer(json_size(tests), 1, 'explicit dispatcher remains one report entry')
    entry = json_element(tests, 1)
    field = json_member(entry, 'name')
    call assert_equal_string(json_string_value(field), 'test_dispatcher', 'explicit dispatcher name')
    field = json_member(report, 'exit_code')
    call assert_equal_integer(nint(json_number_value(field)), 3, 'dispatcher observes missing name')

    call write_text(join_path(scratch, 'test/test_alpha.f90'), &
        case_source('test_alpha', 'alpha-updated', .true.))
    call expect_cases(words([character(len=32) :: 'test', 'test_alpha']), &
        [character(len=32) :: 'test_alpha'], [character(len=32) :: 'test_alpha'], &
        [character(len=64) :: 'alpha-updated'])
    call expect_cases(words([character(len=32) :: 'test', '--all']), initial_names, initial_names, changed_values)

    call list_add(listing_args, '-1')
    call list_add(listing_args, join_path(scratch, 'build/fo/bin'))
    call list_add(listing_env, 'LC_ALL=C')
    call run_external('/bin/ls', listing_args, scratch, result, listing_env)
    call assert_process_ok(result, 'dispatcher routes to one built binary')
    binary_listing = 'test_dispatcher' // new_line('a') // 'test_plain' // new_line('a')
    call assert_equal_string(result%stdout, binary_listing, &
        'dispatcher has one shared executable and one plain executable')

    marker_manifest = '[[test]]' // new_line('a') // 'name = "test_marker_only"' // &
        new_line('a') // 'source-dir = "test"' // new_line('a') // &
        'main = "nested/marker_only.f90"' // new_line('a')
    call append_text(join_path(scratch, 'fpm.toml'), marker_manifest)
    call write_text(join_path(scratch, 'test/nested/marker_only.f90'), &
        '! fo: dispatcher' // new_line('a') // '! no build unit' // new_line('a'))
    call list_add(regex_environment, 'FO_SCAN_FALLBACK=regex')
    call expect_rejected('test_marker_only', regex_environment)
    call expect_cases(words([character(len=32) :: 'test', '--all']), initial_names, initial_names, changed_values, &
        regex_environment)
    call expect_cases(words([character(len=32) :: 'test', '--all']), initial_names, initial_names, changed_values, &
        regex_environment)
    call remove_path(join_path(scratch, 'test/nested/marker_only.f90'))

    call move_path(join_path(scratch, 'test'), join_path(scratch, 'checks'))
    manifest = read_text(join_path(scratch, 'fpm.toml'))
    rewritten = replace_all(manifest, 'source-dir = "test"', 'source-dir = "checks"')
    rewritten = rewritten // new_line('a') // '[build]' // new_line('a') // &
        'test-dir = "checks"' // new_line('a')
    call write_text(join_path(scratch, 'fpm.toml'), rewritten)
    call expect_cases(words([character(len=32) :: 'test', 'test_beta']), &
        [character(len=32) :: 'test_beta'], [character(len=32) :: 'test_beta'], &
        [character(len=64) :: 'beta-original'])
    call expect_cases(words([character(len=32) :: 'test', '--all']), initial_names, initial_names, changed_values)
    call expect_rejected('test_module_only')
    call expect_rejected('test_flat_module')
    call expect_rejected('test_flat_source')

    call write_text(join_path(scratch, 'checks/suite_entry.f90'), &
        '! fo: dispatcher' // new_line('a') // 'module suite_entry' // &
        new_line('a') // 'end module' // new_line('a'))
    call remove_path(join_path(scratch, 'checks/test_legacy.f90'))
    call expect_rejected('test_beta')
    call expect_rejected('test_alpha')
    call expect_rejected('test_dispatcher')
    call expect_cases(words([character(len=32) :: 'test', '--all']), plain_name, plain_name, plain_value)
    call expect_cases(words([character(len=32) :: 'test', '--all']), plain_name, plain_name, plain_value)
    call expect_cases(words([character(len=32) :: 'test', '--random', '1', '--seed', '42']), &
        plain_name, plain_name, plain_value)
    call expect_cases(words([character(len=32) :: 'test', '--only-changed']), plain_name, plain_name, plain_value)
    call remove_path(join_path(scratch, 'checks/suite_entry.f90'))
    call expect_rejected('test_beta')
    call expect_rejected('test_does_not_exist')

    call append_text(join_path(scratch, 'fpm.toml'), &
        '[[test]]' // new_line('a') // 'name = "test_probe_slow"' // new_line('a') // &
        'source-dir = "checks"' // new_line('a') // &
        'main = "nested/slow_entry.f90"' // new_line('a'))
    call write_lines(join_path(scratch, 'checks/nested/slow_entry.f90'), &
        [character(len=128) :: 'program slow_entry', 'implicit none', 'integer :: unit', &
        "open(newunit=unit, file='test_probe_slow.receipt', status='replace')", &
        "write(unit, '(a)') 'slow-executed'", 'close(unit)', 'end program'])
    call expect_slow_rejected(words([character(len=32) :: 'test_probe_slow']))
    call expect_slow_rejected(words([character(len=32) :: 'test_plain', 'test_probe_slow']))
    call expect_slow_rejected(words([character(len=32) :: 'test_probe_slow', 'test_plain']))
    call expect_cases(words([character(len=32) :: 'test', '--all', 'test_probe_slow']), &
        [character(len=32) :: 'test_probe_slow'], [character(len=32) :: 'test_probe_slow'], &
        [character(len=64) :: 'slow-executed'])
    call expect_slow_rejected(words([character(len=32) :: 'test_probe_slow']))
    call expect_cases(words([character(len=32) :: 'test', '--all', 'test_plain', 'test_probe_slow']), &
        [character(len=32) :: 'test_plain', 'test_probe_slow'], &
        [character(len=32) :: 'test_plain', 'test_probe_slow'], &
        [character(len=64) :: 'plain-original', 'slow-executed'])
    call expect_cases(words([character(len=32) :: 'test']), plain_name, plain_name, plain_value)

    call remove_path(join_path(scratch, 'checks/test_plain.f90'))
    call expect_empty_random_rejected(.false.)
    call expect_cases(words([character(len=32) :: 'test', '--all', '--random', '3', '--seed', '42']), &
        [character(len=32) :: 'test_probe_slow'], [character(len=32) :: 'test_probe_slow'], &
        [character(len=64) :: 'slow-executed'])
    call expect_cases(words([character(len=32) :: 'test', '--all', '--random', '3', '--seed', '42']), &
        [character(len=32) :: 'test_probe_slow'], [character(len=32) :: 'test_probe_slow'], &
        [character(len=64) :: 'slow-executed'])
    call remove_path(join_path(scratch, 'checks/nested/slow_entry.f90'))
    call remove_if_present(join_path(scratch, 'test_probe_slow.receipt'))
    call expect_empty_random_rejected(.false.)
    call expect_empty_random_rejected(.true.)
    call remove_tree(scratch)
    write(*, '(a)') 'dispatcher-cli: routing, warm receipts, aliases, roots, eligibility and slow gates pass'

contains

    function words(items) result(list)
        character(len=*), intent(in) :: items(:)
        type(string_list_t) :: list
        integer :: i

        do i = 1, size(items)
            call list_add(list, trim(items(i)))
        end do
    end function words

    subroutine invoke_fo(arguments, result, environment)
        type(string_list_t), intent(in) :: arguments
        type(process_result_t), intent(out) :: result
        type(string_list_t), optional, intent(in) :: environment
        if (present(environment)) then
            call run_fo(driver, arguments, scratch, join_path(scratch, '.fo-cache'), &
                result, environment)
        else
            call run_fo(driver, arguments, scratch, join_path(scratch, '.fo-cache'), result)
        end if
    end subroutine invoke_fo

    subroutine expect_cases(arguments, expected_names, receipt_names, receipt_values, environment)
        type(string_list_t), intent(in) :: arguments
        character(len=*), intent(in) :: expected_names(:), receipt_names(:), receipt_values(:)
        type(string_list_t), optional, intent(in) :: environment
        type(string_list_t) :: command
        type(process_result_t) :: child
        type(json_value_t) :: document, entries, item, field, summary
        logical, allocatable :: found(:)
        integer :: i, j
        character(:), allocatable :: receipt_path, expected

        call assert_equal_integer(size(receipt_names), size(receipt_values), &
            'receipt names and values have equal lengths')
        do i = 1, size(all_cases)
            call remove_if_present(join_path(scratch, trim(all_cases(i)) // '.receipt'))
        end do
        call list_extend(command, arguments)
        call list_add(command, '--json')
        call invoke_fo(command, child, environment)
        call assert_process_ok(child, 'selected dispatcher tests succeed')
        call parse_json_report(child, document, 'dispatcher test report')
        entries = json_member(document, 'tests')
        call assert_equal_integer(json_size(entries), size(expected_names), &
            'dispatcher report contains exactly expected tests')
        allocate(found(size(expected_names)))
        found = .false.
        do i = 1, json_size(entries)
            item = json_element(entries, i)
            field = json_member(item, 'name')
            expected = json_string_value(field)
            field = json_member(item, 'status')
            call assert_equal_string(json_string_value(field), 'pass', &
                'dispatcher entry passes')
            do j = 1, size(expected_names)
                if (expected == trim(expected_names(j))) found(j) = .true.
            end do
        end do
        do i = 1, size(found)
            call assert_true(found(i), 'expected report name: ' // trim(expected_names(i)))
        end do
        summary = json_member(document, 'summary')
        field = json_member(summary, 'failed')
        call assert_equal_integer(nint(json_number_value(field)), 0, 'report has no failures')
        do i = 1, size(receipt_names)
            receipt_path = join_path(scratch, trim(receipt_names(i)) // '.receipt')
            call assert_file_equals(receipt_path, trim(receipt_values(i)) // new_line('a'), &
                'dispatcher execution receipt: ' // trim(receipt_names(i)))
        end do
        do i = 1, size(all_cases)
            do j = 1, size(receipt_names)
                if (trim(all_cases(i)) == trim(receipt_names(j))) exit
            end do
            if (j > size(receipt_names)) then
                call assert_file_absent(join_path(scratch, trim(all_cases(i)) // '.receipt'), &
                    'unselected dispatcher case did not execute')
            end if
        end do
    end subroutine expect_cases

    subroutine expect_rejected(name, environment)
        character(len=*), intent(in) :: name
        type(string_list_t), optional, intent(in) :: environment
        type(string_list_t) :: command
        type(process_result_t) :: child

        call list_add(command, 'test')
        call list_add(command, name)
        call list_add(command, '--json')
        call invoke_fo(command, child, environment)
        call assert_true(child%exit_code /= 0, 'unknown target fails: ' // name)
        call assert_contains(child%stderr, 'fo: unknown test: ' // name, &
            'unknown target diagnostic: ' // name)
    end subroutine expect_rejected

    subroutine expect_slow_rejected(names)
        type(string_list_t), intent(in) :: names
        type(string_list_t) :: command
        type(process_result_t) :: child
        integer :: i

        do i = 1, size(all_cases)
            call remove_if_present(join_path(scratch, trim(all_cases(i)) // '.receipt'))
        end do
        call list_add(command, 'test')
        call list_extend(command, names)
        call list_add(command, '--json')
        call invoke_fo(command, child)
        call assert_true(child%exit_code /= 0, 'explicit slow selection needs --all')
        call assert_contains(child%stderr, 'fo: slow test test_probe_slow requires --all', &
            'slow test requires explicit all selection')
        call assert_not_contains(child%stderr, 'unknown test', 'slow alias is recognized')
        do i = 1, size(all_cases)
            call assert_file_absent(join_path(scratch, trim(all_cases(i)) // '.receipt'), &
                'reject slow selection before running any test')
        end do
    end subroutine expect_slow_rejected

    subroutine expect_empty_random_rejected(include_all)
        logical, intent(in) :: include_all
        type(string_list_t) :: command
        type(process_result_t) :: child
        type(json_value_t) :: document, field
        integer :: i

        do i = 1, size(all_cases)
            call remove_if_present(join_path(scratch, trim(all_cases(i)) // '.receipt'))
        end do
        call list_add(command, 'test')
        call list_add(command, '--random')
        call list_add(command, '3')
        call list_add(command, '--seed')
        call list_add(command, '42')
        if (include_all) call list_add(command, '--all')
        call list_add(command, '--json')
        call invoke_fo(command, child)
        call assert_true(child%exit_code /= 0, 'empty random selection fails')
        call assert_contains(child%stderr, 'fo: no eligible tests for --random', &
            'empty random selection diagnostic')
        if (.not. include_all) call assert_contains(child%stderr, 'use --all', &
            'random selection explains slow test opt-in')
        if (len_trim(child%stdout) > 0) then
            call parse_json_report(child, document, 'empty random result')
            field = json_member(document, 'exit_code')
            call assert_true(nint(json_number_value(field)) /= 0, &
                'empty random selection cannot report success')
        end if
        do i = 1, size(all_cases)
            call assert_file_absent(join_path(scratch, trim(all_cases(i)) // '.receipt'), &
                'empty random selection executes nothing')
        end do
    end subroutine expect_empty_random_rejected

    function case_source(name, value, marker) result(source)
        character(len=*), intent(in) :: name, value
        logical, intent(in) :: marker
        character(:), allocatable :: source

        source = ''
        if (marker) source = '! fo: dispatcher' // new_line('a')
        source = source // 'module ' // trim(name) // '_case' // new_line('a') // &
            'implicit none' // new_line('a') // 'contains' // new_line('a') // &
            'subroutine run_case()' // new_line('a') // 'integer :: unit' // new_line('a') // &
            "open(newunit=unit, file='" // trim(name) // ".receipt', status='replace')" // &
            new_line('a') // "write(unit, '(a)') '" // trim(value) // "'" // new_line('a') // &
            'close(unit)' // new_line('a') // 'end subroutine run_case' // new_line('a') // &
            'end module ' // trim(name) // '_case' // new_line('a')
    end function case_source

    function replace_all(source, old, replacement) result(output)
        character(len=*), intent(in) :: source, old, replacement
        character(:), allocatable :: output
        integer :: position, match_at

        output = ''
        position = 1
        do while (position <= len(source))
            match_at = index(source(position:), old)
            if (match_at == 0) then
                output = output // source(position:)
                exit
            end if
            output = output // source(position:position + match_at - 2) // replacement
            position = position + match_at - 1 + len(old)
        end do
    end function replace_all

    subroutine remove_if_present(path)
        character(len=*), intent(in) :: path

        if (file_exists(path)) call remove_path(path)
    end subroutine remove_if_present

end program test_dispatcher_cli
