program test_native_manifest_runtime_cli
    use fo_test_harness, only: string_list_t, process_result_t, list_add, &
        make_scratch, write_text, run_process, assert_process_ok, assert_true, &
        assert_equal_string, finish_assertions
    use fo_test_cli, only: resolve_driver, run_fo, parse_json_report
    use fo_test_json, only: json_value_t, json_member, json_element, &
        json_string_value, json_size
    implicit none
    character(:), allocatable :: driver, scratch, project
    character(len=*), parameter :: nl = new_line('a')
    type(string_list_t) :: environment
    integer :: shape

    call resolve_driver(driver)
    call make_scratch('fo-native-manifest-runtime', scratch)
    call list_add(environment, 'FO_BACKEND=fpm')
    call list_add(environment, 'FO_JOBS=2')
    do shape = 1, 5
        call contained_main(shape)
    end do
    call version_macros()
    call helper_interface()
    call finish_assertions()
contains
    subroutine contained_main(kind)
        integer, intent(in) :: kind
        character(:), allocatable :: body, prefix, number, expected
        character(len=20) :: text
        integer :: phase, delta, phase_count
        type(string_list_t) :: args
        type(process_result_t) :: result

        write(text, '(i0)') kind
        project = scratch//'/main-'//trim(text)
        call write_text(project//'/fpm.toml', 'name="main_contained"'//nl)
        call write_text(project//'/src/external.f90', &
            '! External procedure with its own internal procedure'//nl// &
            'integer function external_payload()'//nl//'external_payload=23'//nl// &
            'contains'//nl//'integer function unused_helper()'//nl// &
            'unused_helper=0'//nl//'end function'//nl//'end function'//nl)
        prefix = 'program main'//nl
        select case (kind)
        case (1)
            prefix = prefix//'call helper()'//nl//'contains'//nl// &
                'subroutine helper()'//nl
        case (3)
            prefix = "print '(i0)',helper()"//nl//'contains'//nl// &
                'integer function helper()'//nl
        case (4)
            prefix = "program main; print '(i0)',helper(); contains"//nl// &
                'integer function helper()'//nl
        case default
            if (kind == 5) prefix = 'program ordinary'//nl
            prefix = prefix//"print '(i0)',helper()"//nl//'contains'//nl// &
                'integer function helper()'//nl
        end select
        body = 'interface'//nl//'integer function external_payload()'//nl// &
            'end function'//nl//'end interface'//nl
        phase_count = 1
        if (kind == 1) phase_count = 4
        do phase = 1, phase_count
            delta = 0
            if (phase == 3) delta = 8
            write(text, '(i0)') delta
            number = trim(text)
            if (phase /= 2) then
                if (kind == 1) then
                    call write_text(project//'/app/main.f90', prefix//body// &
                        "print '(i0)',external_payload()+"//number//nl// &
                        'end subroutine'//nl//'end program'//nl)
                else
                    call write_text(project//'/app/main.f90', prefix//body// &
                        'helper=external_payload()+'//number//nl// &
                        'end function'//nl//'end program'//nl)
                end if
            end if
            write(text, '(i0)') 23 + delta
            expected = trim(text)
            args = string_list_t()
            call list_add(args, 'exec')
            call list_add(args, 'main_contained')
            call run_fo(driver, args, project, scratch//'/cache', result, environment)
            call assert_process_ok(result, 'contained main compiles, links and runs')
            call assert_equal_string(without_line_ending(result%stdout), expected, &
                'main shape and warm source edits retain independent payload')
        end do
    end subroutine contained_main

    function without_line_ending(text) result(output)
        character(len=*), intent(in) :: text
        character(:), allocatable :: output
        integer :: finish
        finish = len_trim(text)
        do while (finish > 0)
            if (index(achar(10)//achar(13), text(finish:finish)) == 0) exit
            finish = finish - 1
        end do
        output = text(:finish)
    end function without_line_ending

    subroutine version_macros()
        character(:), allocatable :: macro, version, header, manifest, source
        integer :: kind, phase, phase_count

        source = 'program test_version_macro'//nl//'implicit none'//nl// &
            'character(64)::observed,expected'//nl// &
            '#define STRINGIFY_START(X) "&'//nl//'#define STRINGIFY_END(X) &X"'//nl// &
            'observed=STRINGIFY_START(PACKAGE_VERSION)'//nl// &
            'STRINGIFY_END(PACKAGE_VERSION)'//nl// &
            'call get_command_argument(1,expected)'//nl// &
            'if(trim(observed)/=trim(expected))error stop "compiled version"'//nl// &
            'if(UNCHANGED/=7)error stop "unrelated macro"'//nl//'end program'//nl
        do kind = 1, 4
            macro = 'PACKAGE_VERSION'
            if (kind == 4) macro = macro//repeat('X', 100)
            project = scratch//'/version-'//macro
            header = '[preprocess.cpp]'//nl//'macros'
            if (kind == 3) header = '[preprocess]'//nl//'cpp.macros'
            if (kind == 4) then
                call write_text(project//'/test/test_version_macro.f90', &
                    replace_macro(source, macro))
            else
                call write_text(project//'/test/test_version_macro.f90', source)
            end if
            phase_count = 1
            if (kind == 1) phase_count = 4
            do phase = 1, phase_count
                version = '0.13.0'
                if (kind == 2) version = '0'
                if (kind == 3) version = '2.3.4'
                if (kind == 4) version = '8.9.10'
                if (phase == 3) version = '4.5.6'
                manifest = 'name="version_macro"'//nl
                if (kind /= 2 .or. phase == 3) &
                    manifest = manifest//'version="'//version//'"'//nl
                manifest = manifest//header//'=["'//macro// &
                    '={version}","UNCHANGED=7"]'//nl// &
                    '[extra.fo.test-args]'//nl//'test_version_macro=["'// &
                    version//'"]'//nl
                if (phase /= 2) call write_text(project//'/fpm.toml', manifest)
                call named_test('test_version_macro', .true.)
            end do
        end do
    end subroutine version_macros

    function replace_macro(source, macro) result(output)
        character(len=*), intent(in) :: source, macro
        character(:), allocatable :: output
        integer :: at, first
        output = ''
        first = 1
        do
            at = index(source(first:), 'PACKAGE_VERSION')
            if (at == 0) exit
            at = first + at - 1
            output = output//source(first:at - 1)//macro
            first = at + len('PACKAGE_VERSION')
        end do
        output = output//source(first:)
    end function replace_macro

    subroutine helper_interface()
        character(:), allocatable :: source, value, leaf, reference
        type(string_list_t) :: args
        type(process_result_t) :: result
        integer :: phase

        project = scratch//'/explicit-roots'
        call write_text(project//'/fpm.toml', &
            'name="explicit_roots"'//nl//'[build]'//nl//'auto-tests=false'//nl// &
            '[dev-dependencies]'//nl//'provider={path="provider"}'//nl// &
            '[[test]]'//nl//'name="nested-alpha"'//nl// &
            'source-dir="test/alpha"'//nl//'main="main.f90"'//nl// &
            '[[test]]'//nl//'name="outside-beta"'//nl// &
            'source-dir="checks/beta"'//nl//'main="main.f90"'//nl)
        call write_text(project//'/provider/fpm.toml', 'name="provider"'//nl)
        call write_text(project//'/provider/src/provider.f90', &
            'module provider'//nl//'integer,parameter::provider_value=5'//nl// &
            'end module'//nl)
        call write_text(project//'/src/library.f90', &
            'module library'//nl//'integer,parameter::library_value=3'//nl// &
            'end module'//nl)
        call write_text(project//'/test/alpha/unregistered.f90', &
            'program unregistered'//nl// &
            'error stop "unregistered program must not execute"'//nl//'end program'//nl)
        source = 'use library,only:library_value'//nl// &
            'use provider,only:provider_value'//nl
        call write_text(project//'/test/alpha/main.f90', 'program alpha'//nl// &
            source//'use helper,only:helper_value'//nl// &
            'if(library_value+provider_value+helper_value/=1'// &
            '5)error stop "alpha15"'//nl// &
            'end program'//nl)
        call write_text(project//'/checks/beta/main.f90', 'program beta'//nl//source// &
            'if(library_value+provider_value+7/=15)error stop "beta15"'//nl// &
            'end program'//nl)
        leaf = project//'/test/alpha/helper.f90'
        reference = scratch//'/helper-reference'
        call write_text(leaf, 'module helper'//nl// &
            'integer,parameter::helper_value=7'//nl//'end module'//nl)
        call list_add(args, 'cp')
        call list_add(args, '-p')
        call list_add(args, leaf)
        call list_add(args, reference)
        call run_process(args, scratch, result)
        call assert_process_ok(result, 'save independent helper timestamp')
        do phase = 1, 4
            value = '7'
            if (phase == 3) value = '9'
            if (phase >= 3) then
                call write_text(leaf, 'module helper'//nl// &
                    'integer,parameter::helper_value='//value//nl//'end module'//nl)
                args = string_list_t()
                call list_add(args, 'touch')
                call list_add(args, '-r')
                call list_add(args, reference)
                call list_add(args, leaf)
                call run_process(args, scratch, result)
                call assert_process_ok(result, 'restore helper interface timestamp')
            end if
            call helper_pair(phase /= 3)
        end do
        call named_test('missing-case', .false.)
    end subroutine helper_interface

    subroutine helper_pair(alpha_pass)
        logical, intent(in) :: alpha_pass
        type(string_list_t) :: args
        type(process_result_t) :: result
        type(json_value_t) :: report, tests, entry
        character(:), allocatable :: name, expected
        integer :: i
        logical :: alpha_seen, beta_seen

        call list_add(args, 'test')
        call list_add(args, 'nested-alpha')
        call list_add(args, 'outside-beta')
        call list_add(args, '--json')
        call run_fo(driver, args, project, scratch//'/cache', result, environment)
        call assert_true((result%exit_code == 0) .eqv. alpha_pass, &
            'selected root/helper edit changes the public runtime verdict')
        call parse_json_report(result, report, 'multiple explicit source-root report')
        tests = json_member(report, 'tests')
        call assert_true(json_size(tests) == 2, &
            'both explicit tests run without extra mains')
        alpha_seen = .false.
        beta_seen = .false.
        do i = 1, json_size(tests)
            entry = json_element(tests, i)
            name = json_string_value(json_member(entry, 'name'))
            expected = 'pass'
            if (name == 'nested-alpha') then
                alpha_seen = .true.
                if (.not. alpha_pass) expected = 'fail'
            else if (name == 'outside-beta') then
                beta_seen = .true.
            else
                call assert_true(.false., 'unregistered main was included: '//name)
            end if
            call assert_equal_string(json_string_value(json_member(entry, 'status')), &
                expected, 'independent per-root runtime verdict: '//name)
        end do
        call assert_true(alpha_seen .and. beta_seen, &
            'both registered names retain receipts')
    end subroutine helper_pair

    subroutine named_test(name, expected_pass)
        character(len=*), intent(in) :: name
        logical, intent(in) :: expected_pass
        type(string_list_t) :: args
        type(process_result_t) :: result
        type(json_value_t) :: report, tests, entry

        call list_add(args, 'test')
        call list_add(args, name)
        call list_add(args, '--json')
        call run_fo(driver, args, project, scratch//'/cache', result, environment)
        if (expected_pass) then
            call assert_process_ok(result, 'public native runtime oracle: '//name)
            call parse_json_report(result, report, 'native runtime report')
            tests = json_member(report, 'tests')
            call assert_true(json_size(tests) == 1, &
                'only named registered test executes')
            entry = json_element(tests, 1)
            call assert_equal_string(json_string_value(json_member(entry, 'name')), &
                name, 'manifest registered name survives native execution')
            call assert_equal_string(json_string_value(json_member(entry, 'status')), &
                'pass', 'compiled runtime oracle independently passes')
        else
            call assert_true(result%exit_code /= 0, &
                'negative runtime/name oracle: '//name)
        end if
    end subroutine named_test
end program test_native_manifest_runtime_cli
