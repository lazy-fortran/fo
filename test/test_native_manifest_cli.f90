program test_native_manifest_cli
    use fo_test_harness, only: process_result_t, string_list_t, list_add
    use fo_test_harness, only: make_scratch, join_path, write_text, remove_tree
    use fo_test_harness, only: assert_process_ok, assert_true, assert_contains
    use fo_test_harness, only: assert_file_equals, finish_assertions
    use fo_test_cli, only: resolve_driver, run_fo, run_external
    implicit none

    character(:), allocatable :: driver, scratch, root
    character(len=1024) :: reference, previous_driver
    character(len=1), parameter :: nl = new_line('a')
    integer :: status
    type(process_result_t) :: result, independent
    type(string_list_t) :: args, environment

    call resolve_driver(driver)
    call make_scratch('fo-native-manifest', scratch)
    root = join_path(scratch, 'naming')
    call list_add(environment, 'FO_JOBS=2')
    reference = ''
    call get_environment_variable('FO_FPM_REFERENCE', reference, status=status)
    if (status /= 0) reference = ''

    call naming_case('false', 'arbitrary', .true.)
    call naming_case('true', 'naming_probe', .true.)
    call naming_case('true', 'naming_probe__value', .true.)
    call naming_case('true', 'NAMING_PROBE__VALUE', .true.)
    call naming_case('true', 'naming_probe_value', .false.)
    call naming_case('true', 'naming_probe__', .false.)
    call naming_case('true', 'foreign', .false.)
    call naming_case('true', 'naming_probe', .false., 'foreign')
    call naming_case('true', 'naming_probe', .true., 'naming_probe__second')
    call naming_case('"Custom"', 'custom', .true.)
    call naming_case('"Custom"', 'custom_value', .true.)
    call naming_case('"Custom"', 'naming_probe__value', .true.)
    call naming_case('"Custom"', 'custom_', .false.)
    call naming_case('"Custom"', 'customvalue', .false.)
    call naming_case('"bad_prefix"', 'arbitrary', .false.)
    call naming_case('"9bad"', 'arbitrary', .false.)
    call naming_case('""', 'arbitrary', .false.)
    call naming_case('42', 'arbitrary', .false.)
    previous_driver = ''
    call get_environment_variable('FO_PREVIOUS_DRIVER', previous_driver, status=status)
    if (status == 0 .and. len_trim(previous_driver) > 0) then
        call write_text(join_path(root, 'fpm.toml'), manifest('true'))
        call write_module('foreign')
        args = string_list_t()
        call list_add(args, 'build')
        call run_fo(trim(previous_driver), args, root, join_path(scratch, 'cache'), &
            independent, environment)
        call assert_process_ok(independent, &
            'previous driver seeds obsolete warm artifacts')
        call expect_build(.false., 'module')
    end if
    call dependency_naming_case('false', 'provider', .true.)
    call dependency_naming_case('false', 'foreign', .false.)
    call dependency_naming_case('"Custom"', 'custom_value', .true.)
    call dependency_naming_case('"Custom"', 'foreign', .false.)

    call source_policy_case()
    call include_directory_case()
    call parallel_package_case()

    ! Test support modules are exempt from the root package naming policy.
    ! Start an independent reference project: FPM retains removed dependencies
    ! in its lock/model across some manifest changes.
    root = join_path(scratch, 'test_naming')
    call write_text(join_path(root, 'fpm.toml'), manifest('true') // &
        '[[test]]' // nl // 'name = "named_case"' // nl // &
        'source-dir = "checks"' // nl // 'main = "case.f90"' // nl)
    call write_module('naming_probe')
    call write_text(join_path(root, 'checks/support.f90'), &
        'module unrelated_test_support' // nl // 'implicit none' // nl // &
        'integer, parameter :: payload = 23' // nl // &
        'end module unrelated_test_support' // nl)
    call write_text(join_path(root, 'checks/case.f90'), &
        'program test_case' // nl // &
        'use unrelated_test_support, only: payload' // nl // &
        'implicit none' // nl // 'integer :: unit' // nl // &
        "open(newunit=unit,file='naming.receipt',status='replace')" // nl // &
        "write(unit,'(i0)') payload" // nl // 'close(unit)' // nl // &
        'end program test_case' // nl)
    args = string_list_t()
    call list_add(args, 'test')
    call list_add(args, 'named_case')
    if (len_trim(reference) > 0) then
        call run_external(trim(reference), args, root, independent, environment)
        call assert_process_ok(independent, 'pinned FPM exempts test module names')
    end if
    call run_fo(driver, args, root, join_path(scratch, 'cache'), &
            result, environment)
    call assert_process_ok(result, 'Fo exempts named test support module names')
    call assert_file_equals(join_path(root, 'naming.receipt'), '23' // nl, &
        'exempt test module executes its independent payload')

    call write_text(join_path(root, 'fpm.toml'), &
        'name = "naming-probe"' // nl // '[fortran]' // nl // &
        'source-form = "unknown"' // nl)
    call expect_build(.false., 'source-form')
    call write_text(join_path(root, 'fpm.toml'), &
        'name = "naming-probe"' // nl // '[build]' // nl // &
        'unknown-feature = true' // nl)
    call expect_build(.false., 'unsupported [build] field')
    call write_text(join_path(root, 'fpm.toml'), &
        'name = "naming-probe"' // nl // '[build]' // nl // &
        'auto-tests = "true"' // nl)
    call reference_build(.false., 'pinned FPM rejects quoted boolean discovery policy')
    call expect_build(.false., 'requires a boolean')
    call write_text(join_path(root, 'app/main.f90'), &
        'program probe' // nl // 'implicit none' // nl // &
        "print '(i0)', 19" // nl // 'end program probe' // nl)
    call write_text(join_path(root, 'fpm.toml'), &
        'name = "naming-probe"' // nl // '[[executable]]' // nl // &
        'name = "probe"' // nl // 'link = ["m"]' // nl)
    call reference_build(.true., 'FPM supports target-local link')
    call expect_build(.false., 'target-local link is unsupported')
    call write_text(join_path(scratch, 'provider/fpm.toml'), &
        'name = "provider"' // nl)
    call write_text(join_path(scratch, 'provider/src/provider.f90'), &
        'module provider' // nl // 'implicit none' // nl // &
        'integer, parameter :: payload = 29' // nl // 'end module provider' // nl)
    call write_text(join_path(root, 'fpm.toml'), &
        'name = "naming-probe"' // nl // '[[executable]]' // nl // &
        'name = "probe"' // nl // '[executable.dependencies]' // nl // &
        'provider = { path = "../provider" }' // nl)
    call reference_build(.true., 'FPM supports target-local dependencies')
    call expect_build(.false., 'unsupported [executable.dependencies]')
    call write_text(join_path(root, 'fpm.toml'), &
        'name = "naming-probe"' // nl // '[library]' // nl // &
        'include-dir = [42]' // nl)
    call reference_build(.false., 'FPM rejects invalid include directories')
    call expect_build(.false., 'include-dir requires a quoted path')
    call write_text(join_path(root, 'fpm.toml'), &
        'name = "naming-probe"' // nl // '[test.dependencies]' // nl // &
        'provider = { path = "../provider" }' // nl)
    call expect_build(.false., 'requires a preceding [[test]]')

    call finish_assertions(retain_failed_scratch=.true.)

contains

    subroutine parallel_package_case()
        character(:), allocatable :: saved_root, package, source, form
        character(len=32) :: package_name, module_name, value
        integer :: i, j

        saved_root = root
        root = join_path(scratch, 'parallel_policy')
        call write_text(join_path(root, 'fpm.toml'), &
            'name = "parallel_policy"' // nl // '[dependencies]' // nl // &
            'package1 = { path = "package1" }' // nl // &
            'package2 = { path = "package2" }' // nl // &
            'package3 = { path = "package3" }' // nl // &
            '[[executable]]' // nl // 'name = "probe"' // nl)
        ! Twelve independent providers share one parallel DAG level; package
        ! policies and macro lengths differ. Pinned FPM supplies the oracle.
        source = 'program probe' // nl
        do i = 1, 3
            write(package_name, '(a,i0)') 'package', i
            package = join_path(root, trim(package_name))
            select case (i)
            case (1)
                form = 'fixed'
                value = '11'
            case (2)
                form = 'free'
                value = '137'
            case (3)
                form = 'default'
                value = '2003'
            end select
            call write_text(join_path(package, 'fpm.toml'), &
                'name = "' // trim(package_name) // '"' // nl // &
                '[fortran]' // nl // 'source-form = "' // form // '"' // nl // &
                '[preprocess.cpp]' // nl // 'suffixes = ["f90"]' // nl // &
                'macros = ["PAYLOAD=' // trim(value) // '"]' // nl)
            do j = 1, 4
                write(module_name, '(a,a,i0)') trim(package_name), '_', j
                call write_text(join_path(package, &
                    'src/' // trim(module_name) // '.f90'), &
                    '      module ' // trim(module_name) // nl // &
                    '      implicit none' // nl // &
                    '      integer, parameter :: value = PAYLOAD' // nl // &
                    '      end module ' // trim(module_name) // nl)
                source = source // 'use ' // trim(module_name) // &
                    ', only: value_' // trim(module_name) // ' => value' // nl
            end do
        end do
        source = source // 'implicit none' // nl // 'integer :: total' // nl // &
            'total = 0' // nl
        do i = 1, 3
            do j = 1, 4
                write(module_name, '(a,i0,a,i0)') 'package', i, '_', j
                source = source // 'total = total + value_' // &
                    trim(module_name) // nl
            end do
        end do
        source = source // "print '(i0)', total" // nl // 'end program probe' // nl
        call write_text(join_path(root, 'app/main.f90'), source)
        call parallel_package_run('8604')
        call remove_tree(join_path(root, 'build/fo/obj'))
        call remove_tree(join_path(root, 'build/fo/mod'))
        call parallel_package_run('8604')
        call write_text(join_path(root, 'package2/fpm.toml'), &
            'name = "package2"' // nl // '[fortran]' // nl // &
            'source-form = "free"' // nl // '[preprocess.cpp]' // nl // &
            'suffixes = ["f90"]' // nl // 'macros = ["PAYLOAD=139"]' // nl)
        call parallel_package_run('8612')
        root = saved_root
    end subroutine parallel_package_case

    subroutine parallel_package_run(expected)
        character(len=*), intent(in) :: expected
        type(string_list_t) :: parallel_environment

        call list_add(parallel_environment, 'FO_JOBS=8')
        args = string_list_t()
        call list_add(args, 'run')
        call list_add(args, 'probe')
        if (len_trim(reference) > 0) then
            call run_external(trim(reference), args, root, independent, environment)
            call assert_process_ok(independent, 'pinned FPM mixed package policy runs')
            call assert_contains(independent%stdout, expected // nl, &
                'pinned FPM executes independent mixed policy payload')
        end if
        call run_fo(driver, args, root, join_path(scratch, 'cache'), result, &
            parallel_environment)
        call assert_process_ok(result, 'parallel native mixed package policy runs')
        call assert_true(result%stdout == expected // nl, &
            'parallel native restore preserves package policy and macro payload')
    end subroutine parallel_package_run

    subroutine include_directory_case()
        character(:), allocatable :: saved_root, provider

        saved_root = root
        root = join_path(scratch, 'include_policy')
        provider = join_path(scratch, 'header_provider')
        call write_text(join_path(provider, 'fpm.toml'), &
            'name = "header_provider"' // nl // '[library]' // nl // &
            "include-dir = 'src/proc'" // nl)
        call write_text(join_path(provider, 'src/proc/provider.h'), &
            '#define PROVIDER_VALUE 29' // nl)
        call write_text(join_path(provider, 'src/provider.c'), &
            '#include "provider.h"' // nl // &
            'int provider_value(void) { return PROVIDER_VALUE; }' // nl)
        call write_text(join_path(root, 'fpm.toml'), &
            'name = "include_policy"' // nl // '[dependencies]' // nl // &
            'header_provider = { path = "../header_provider" }' // nl // &
            '[library]' // nl // 'include-dir = [' // nl // &
            ' "headers_one",' // nl // ' "headers_two",' // nl // ']' // nl // &
            '[[executable]]' // nl // 'name = "probe"' // nl)
        call write_text(join_path(root, 'headers_one/root.h'), &
            '#define ROOT_VALUE 7' // nl)
        call write_text(join_path(root, 'headers_two/second.h'), &
            '#define SECOND_VALUE 11' // nl)
        call write_text(join_path(root, 'src/root.c'), &
            '#include "root.h"' // nl // '#include "second.h"' // nl // &
            '#include "provider.h"' // nl // &
            'int root_value(void) {' // nl // &
            'return ROOT_VALUE+SECOND_VALUE+PROVIDER_VALUE; }' // nl)
        call write_text(join_path(root, 'app/main.f90'), &
            'program probe' // nl // 'use iso_c_binding, only: c_int' // nl // &
            'implicit none' // nl // 'interface' // nl // &
            'function root_value() bind(c) result(value)' // nl // &
            'import c_int' // nl // 'integer(c_int) :: value' // nl // &
            'end function' // nl // &
            'function provider_value() bind(c) result(value)' // nl // &
            'import c_int' // nl // 'integer(c_int) :: value' // nl // &
            'end function' // nl // 'end interface' // nl // &
            "print '(i0)', root_value()+provider_value()" // nl // &
            'end program probe' // nl)
        call include_directory_run(76)
        call include_directory_run(76)
        call write_text(join_path(provider, 'src/proc/provider.h'), &
            '#define PROVIDER_VALUE 31' // nl)
        call include_directory_run(80)
        call write_text(join_path(root, 'fpm.toml'), &
            'name = "include_policy"' // nl // '[dependencies]' // nl // &
            'header_provider = { path = "../header_provider" }' // nl // &
            '[library]' // nl // &
            'include-dir = ["headers one", "headers_two"]' // nl // &
            '[[executable]]' // nl // 'name = "probe"' // nl)
        call write_text(join_path(root, 'headers one/root.h'), &
            '#define ROOT_VALUE 17' // nl)
        ! The reference currently omits C include directories containing spaces.
        ! The preceding reference checks establish the include semantics;
        ! this native-only case verifies Fo preserves the declared argv path.
        call include_directory_run(90, use_reference=.false.)
        root = saved_root
    end subroutine include_directory_case

    subroutine include_directory_run(value, use_reference)
        integer, intent(in) :: value
        logical, optional, intent(in) :: use_reference
        character(len=32) :: expected
        logical :: check_reference

        write(expected, '(i0)') value
        args = string_list_t()
        call list_add(args, 'run')
        call list_add(args, 'probe')
        check_reference = len_trim(reference) > 0
        if (present(use_reference)) check_reference = &
            check_reference .and. use_reference
        if (check_reference) then
            call run_external(trim(reference), args, root, independent, environment)
            if (independent%exit_code /= 0) then
                print '(a)', 'include reference stderr: '//independent%stderr
                print '(a)', 'include reference stdout: '//independent%stdout
            end if
            call assert_process_ok(independent, 'pinned FPM builds exported C headers')
            call assert_contains(independent%stdout, trim(expected) // nl, &
                'pinned FPM executes root and dependency include payload')
        end if
        call run_fo(driver, args, root, join_path(scratch, 'cache'), &
            result, environment)
        if (result%exit_code /= 0) then
            print '(a)', 'include Fo stderr: '//result%stderr
            print '(a)', 'include Fo stdout: '//result%stdout
        end if
        call assert_process_ok(result, &
            'Fo builds declared root and dependency C headers')
        call assert_true(result%stdout == trim(expected) // nl, &
            'Fo executes changed exported header payload')
    end subroutine include_directory_run

    subroutine source_policy_case()
        character(:), allocatable :: project, dependency, provider_manifest
        character(:), allocatable :: saved_root

        saved_root = root
        project = join_path(scratch, 'source_policy')
        dependency = join_path(scratch, 'legacy')
        provider_manifest = 'name = "legacy"' // nl // '[fortran]' // nl // &
            'source-form = "fixed"' // nl // 'implicit-typing = true' // nl // &
            'implicit-external = true' // nl
        call write_text(join_path(dependency, 'fpm.toml'), provider_manifest)
        call write_text(join_path(dependency, 'src/legacy.f90'), &
            '      module legacy' // nl // '      contains' // nl // &
            '      integer function value()' // nl // &
            '      integer legacy_external' // nl // &
            '      x = legacy_external()' // nl // '      value = int(' // nl // &
            '     & x)' // nl // '      end function value' // nl // &
            '      end module legacy' // nl // &
            '      integer function legacy_external()' // nl // &
            '      legacy_external = 37' // nl // &
            '      end function legacy_external' // nl)
        call write_text(join_path(project, 'fpm.toml'), &
            'name = "source_policy"' // nl // '[dependencies]' // nl // &
            'legacy = { path = "../legacy" }' // nl // '[[executable]]' // nl // &
            'name = "probe"' // nl // 'source-dir = "app"' // nl // &
            'main = "probe.f"' // nl)
        ! The .f discovery parser needs column7 even with free compiler form.
        ! The expression after column72 independently distinguishes free form.
        call write_text(join_path(project, 'app/probe.f'), &
            '      program probe' // nl // '      use legacy, only: value' // nl // &
            '      implicit none' // nl // "      print '(i0)', " // &
            repeat(' ', 60) // 'value()' // nl // '      end program probe' // nl)
        root = project
        call reference_build(.true., 'pinned FPM applies each package source policy')
        if (len_trim(reference) > 0) then
            args = string_list_t()
            call list_add(args, 'run')
            call list_add(args, 'probe')
            call run_external(trim(reference), args, project, independent, environment)
            call assert_process_ok(independent, &
                'pinned FPM executes mixed source policy')
            call assert_contains(independent%stdout, '37' // nl, &
                'pinned FPM default free form executes the expression after column72')
        end if
        args = string_list_t()
        call list_add(args, 'exec')
        call list_add(args, 'probe')
        call run_fo(driver, args, project, join_path(scratch, 'cache'), result, &
            environment)
        call assert_process_ok(result, &
            'Fo applies default free and dependency fixed form')
        call assert_true(result%stdout == '37' // nl, &
            'dependency implicit typing/external policy executes the value 37')
        call write_text(join_path(dependency, 'fpm.toml'), &
            'name = "legacy"' // nl // '[fortran]' // nl // &
            'source-form = "fixed"' // nl // 'implicit-typing = false' // nl // &
            'implicit-external = true' // nl)
        call reference_build(.false., &
            'pinned FPM rejects undeclared dependency variable')
        call expect_build(.false., 'x')
        call write_text(join_path(dependency, 'fpm.toml'), provider_manifest)
        call expect_build(.true., '')
        root = saved_root
    end subroutine source_policy_case

    function manifest(policy) result(text)
        character(len=*), intent(in) :: policy
        character(:), allocatable :: text
        text = 'name = "naming-probe"' // nl // '[build]' // nl // &
            'module-naming = ' // policy // nl
    end function manifest

    subroutine write_module(name)
        character(len=*), intent(in) :: name
        call write_text(join_path(root, 'src/value.f90'), &
            'module ' // name // nl // 'implicit none' // nl // &
            'integer, parameter :: payload = 17' // nl // &
            'end module ' // name // nl)
    end subroutine write_module

    subroutine naming_case(policy, name, accepted, second_name)
        character(len=*), intent(in) :: policy, name
        logical, intent(in) :: accepted
        character(len=*), intent(in), optional :: second_name
        call write_text(join_path(root, 'fpm.toml'), manifest(policy))
        call write_module(name)
        if (present(second_name)) then
            call write_text(join_path(root, 'src/value.f90'), &
                'module ' // name // nl // 'implicit none' // nl // &
                'end module ' // name // nl // &
                'module ' // second_name // nl // 'implicit none' // nl // &
                'end module ' // second_name // nl)
        end if
        args = string_list_t()
        call list_add(args, 'build')
        if (len_trim(reference) > 0) then
            call run_external(trim(reference), args, root, independent, environment)
            call assert_true((independent%exit_code == 0) .eqv. accepted, &
                'pinned independent FPM verdict: ' // policy // ' / ' // name)
        end if
        call expect_build(accepted, 'module')
    end subroutine naming_case

    subroutine dependency_naming_case(policy, name, accepted)
        character(len=*), intent(in) :: policy, name
        logical, intent(in) :: accepted
        call write_text(join_path(scratch, 'provider/fpm.toml'), &
            'name = "provider"' // nl // '[build]' // nl // &
            'module-naming = ' // policy // nl)
        call write_text(join_path(scratch, 'provider/src/provider.f90'), &
            'module ' // name // nl // 'implicit none' // nl // &
            'integer, parameter :: payload = 29' // nl // &
            'end module ' // name // nl)
        call write_text(join_path(root, 'fpm.toml'), manifest('true') // &
            '[dependencies]' // nl // 'provider = { path = "../provider" }' // nl)
        call write_module('naming_probe')
        call reference_build(accepted, 'pinned FPM applies dependency naming policy')
        call expect_build(accepted, 'module')
    end subroutine dependency_naming_case

    subroutine expect_build(accepted, diagnostic)
        logical, intent(in) :: accepted
        character(len=*), intent(in) :: diagnostic
        args = string_list_t()
        call list_add(args, 'build')
        call run_fo(driver, args, root, join_path(scratch, 'cache'), result, &
            environment)
        if (accepted) then
            call assert_process_ok(result, 'accepted native manifest builds')
        else
            call assert_true(result%exit_code /= 0, 'invalid native manifest fails')
            call assert_contains(result%stderr, diagnostic, &
                'manifest failure is actionable')
        end if
    end subroutine expect_build

    subroutine reference_build(accepted, context)
        logical, intent(in) :: accepted
        character(len=*), intent(in) :: context
        if (len_trim(reference) == 0) return
        args = string_list_t()
        call list_add(args, 'build')
        call run_external(trim(reference), args, root, independent, environment)
        if (accepted) call assert_process_ok(independent, context)
        call assert_true((independent%exit_code == 0) .eqv. accepted, context)
    end subroutine reference_build

end program test_native_manifest_cli
