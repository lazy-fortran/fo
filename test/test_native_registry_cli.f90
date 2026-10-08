program test_native_registry_cli
    use fo_test_harness, only: process_result_t, string_list_t, list_add
    use fo_test_harness, only: make_scratch, join_path, make_directory
    use fo_test_harness, only: make_symlink, write_text, remove_tree, file_exists
    use fo_test_harness, only: assert_process_ok, assert_equal_string, assert_true
    use fo_test_harness, only: assert_contains, finish_assertions
    use fo_test_cli, only: resolve_driver, run_fo
    implicit none
    character(:), allocatable :: driver, scratch, tools, registry, consumer, config
    character(:), allocatable :: path, config_dir, package
    type(process_result_t) :: result
    type(string_list_t) :: args, environment
    character(len=8), parameter :: commands(4) = &
        [character(len=8) :: 'gfortran', 'as', 'ld', 'ar']
    integer :: i

    call resolve_driver(driver)
    call make_scratch('fo-native-registry', scratch)
    tools = join_path(scratch, 'tools')
    registry = join_path(scratch, 'registry')
    consumer = join_path(scratch, 'consumer')
    config_dir = join_path(scratch, 'config')
    config = join_path(config_dir, 'config.toml')
    call make_directory(tools)
    do i = 1, size(commands)
        call find_command(trim(commands(i)), path)
        call make_symlink(path, join_path(tools, trim(commands(i))))
    end do
    call make_directory(join_path(scratch, 'tmp'))
    call list_add(environment, 'PATH='//tools)
    call list_add(environment, 'FO_FPM_CONFIG_FILE='//config)
    call list_add(environment, 'FO_JOBS=2')
    call list_add(environment, 'TMPDIR='//join_path(scratch, 'tmp'))
    call write_text(config, '[registry]'//new_line('a')// &
        'path = "'//registry//'"'//new_line('a'))
    call write_package('leaf', '1.0.0', 5)
    call write_provider('1.9.0', 19)
    call write_provider('1.10.0', 110)
    call write_package('helper_leaf', '1.0.0', 8)
    package = join_path(registry, 'demo/helper/1.0.0')
    call write_text(join_path(package, 'fpm.toml'), &
        'name = "helper"'//new_line('a')//'version = "1.0.0"'// &
        new_line('a')//'[dependencies]'//new_line('a')// &
        'helper_leaf = { namespace = "demo", v = "1.0.0" }'//new_line('a'))
    call write_text(join_path(package, 'src/helper.f90'), &
        'module helper'//new_line('a')// &
        'use helper_leaf, only: helper_leaf_value'//new_line('a')// &
        'contains'//new_line('a')//'integer function helper_value()'// &
        new_line('a')//'helper_value = 7 + helper_leaf_value( &
        )'//new_line('a')// &
        'end function'//new_line('a')//'end module'//new_line('a'))
    call write_text(join_path(consumer, 'app/probe.f90'), &
        'program probe'//new_line('a')//'use provider, only: provider_value'// &
        new_line('a')//"print '(i0)', provider_value()"//new_line('a')// &
        'end program'//new_line('a'))
    call assert_true(.not. file_exists(join_path(tools, 'fpm')), &
        'FPM and network clients are absent from the fixture PATH')

    call write_consumer('provider.v = "1.9.0"')
    call expect_output('24', 'exact version and transitive registry dependency')
    call write_consumer('')
    call expect_output('115', 'latest chooses numerical 1.10.0 over 1.9.0')
    call write_provider('1.10.0', 111)
    call expect_output('116', 'same-version source edits invalidate warm build')
    call write_provider('2.0.0', 200)
    call expect_output('205', 'new version invalidates warm latest selection')
    call write_consumer('provider.v = "01.9.0"')
    call expect_output('24', 'canonical numeric version returns the exact old version')
    call write_consumer('')
    call write_text(join_path(consumer, 'test/check_registry.f90'), &
        'program check_registry'//new_line('a')// &
        'use provider, only: provider_value'//new_line('a')// &
        'use helper, only: helper_value'//new_line('a')// &
        'if ( &
        provider_value( &
        ) + helper_value() /= 220) stop 1'//new_line('a')// &
        'end program'//new_line('a'))
    args = string_list_t()
    call list_add(args, 'test')
    call list_add(args, 'check_registry')
    call run_fo(driver, args, consumer, join_path(scratch, 'cache'), result, &
        environment)
    call assert_process_ok(result, &
        'root dev closure runs; provider dev edge is excluded')

    call write_package('leaf', '2.0.0', 50)
    call write_text(join_path(consumer, 'fpm.toml'), &
        'name = "registry_probe"'//new_line('a')//'[dependencies]'//new_line('a')// &
        'provider.namespace = "demo"'//new_line('a')// &
        'leaf = { namespace = "demo", v = "2.0.0" }'//new_line('a')// &
        '[dev-dependencies]'//new_line('a')// &
        'helper = { namespace = "demo", v = "1.0.0" }'//new_line('a'))
    call expect_output('250', &
        'root selection overrides the transitive registry version')

    call write_package('provider', '1.9.9', 10)
    call write_text(join_path(registry, 'demo/provider/1.9.9/fpm.toml'), &
        'name = "different_package"'//new_line('a')//'version = "1.9.9"'// &
        new_line('a'))
    call write_consumer('provider.v = "1.9.9"')
    call expect_failure('registry package name mismatch', &
        'registry source must have the requested package identity')
    call remove_tree(join_path(registry, 'demo/provider/1.9.9'))

    call write_consumer('provider.v = "7.0.0"')
    call expect_failure('missing registry', 'missing exact registry version')
    call write_text(join_path(consumer, 'fpm.toml'), &
        'name = "registry_probe"'//new_line('a')//'[dependencies]'//new_line('a')// &
        'provider = { v = "1.9.0" }'//new_line('a'))
    call expect_failure('requires namespace', 'missing namespace is explicit')
    call write_text(join_path(consumer, 'fpm.toml'), &
        'name = "registry_probe"'//new_line('a')//'[dependencies]'//new_line('a')// &
        'provider = { namespace = "missing", v = "1.9.0" }'//new_line('a'))
    call expect_failure('missing registry', 'absent namespace is explicit')
    call write_consumer('provider.v = ""')
    call expect_failure('invalid registry v', 'explicit empty version is rejected')
    call write_text(join_path(consumer, 'fpm.toml'), &
        'name = "registry_probe"'//new_line('a')//'[dependencies]'//new_line('a')// &
        'provider = { namespace = "demo", path = "local" }'//new_line('a'))
    call expect_failure('cannot have namespace with path or git', &
        'registry namespace cannot conflict with path source')
    call write_text(join_path(consumer, 'fpm.toml'), &
        'name = "registry_probe"'//new_line('a')//'[dependencies]'//new_line('a')// &
        'provider = { namespace = "demo", git = "unused" }'//new_line('a'))
    call expect_failure('cannot have namespace with path or git', &
        'registry namespace cannot conflict with Git source')
    call write_text(join_path(consumer, 'fpm.toml'), &
        'name = "registry_probe"'//new_line('a')//'[dependencies]'//new_line('a')// &
        'provider = { namespace = "demo", token = "secret" }'//new_line('a'))
    call expect_failure('unsupported dependency field token', &
        'dependency authentication fails explicitly')
    call write_consumer('')
    call write_text(config, '[registry]'//new_line('a')// &
        'path = "'//registry//'"'//new_line('a')//'token = "secret"'//new_line('a'))
    call expect_failure('unsupported registry config/auth key token', &
        'authentication config fails explicitly')
    call write_text(config, '[registry]'//new_line('a')// &
        'url = "https://example.invalid"'//new_line('a'))
    call expect_failure('unsupported registry config/auth key url', &
        'remote registry config fails without network access')
    call remove_tree(config)
    call expect_failure('require [registry] path', 'missing local config is explicit')
    args = string_list_t()
    call list_add(args, 'publish')
    call run_fo(driver, args, consumer, join_path(scratch, 'cache'), result, &
        environment)
    call assert_true(result%exit_code /= 0, 'publishing is explicitly unsupported')
    call assert_contains(result%stderr, 'publish', &
        'publishing diagnostic names request')
    call remove_tree(scratch)
    call finish_assertions('native registry CLI')
contains
    subroutine write_consumer(selector)
        character(len=*), intent(in) :: selector
        call write_text(join_path(consumer, 'fpm.toml'), &
            'name = "registry_probe"'//new_line('a')//'[dependencies]'// &
            new_line('a')//'openmp = "*"'//new_line('a')// &
            'provider.namespace = "demo"'//new_line('a')//trim(selector)// &
            new_line('a')//'[dev-dependencies]'//new_line('a')// &
            'helper = { namespace = "demo", v = "1.0.0" }'//new_line('a'))
    end subroutine write_consumer

    subroutine write_package(name, version, value)
        character(len=*), intent(in) :: name, version
        integer, intent(in) :: value
        character(len=32) :: number
        character(:), allocatable :: root
        write (number, '(i0)') value
        root = join_path(registry, 'demo/'//name//'/'//version)
        call write_text(join_path(root, 'fpm.toml'), 'name = "'//name//'"'// &
            new_line('a')//'version = "'//version//'"'//new_line('a'))
        call write_text(join_path(root, 'src/'//name//'.f90'), &
            'module '//name//new_line('a')//'contains'//new_line('a')// &
            'integer function '//name//'_value()'//new_line('a')// &
            name//'_value = '//trim(number)//new_line('a')//'end function'// &
            new_line('a')//'end module'//new_line('a'))
    end subroutine write_package

    subroutine write_provider(version, value)
        character(len=*), intent(in) :: version
        integer, intent(in) :: value
        character(len=32) :: number
        character(:), allocatable :: root
        call write_package('provider', version, value)
        root = join_path(registry, 'demo/provider/'//version)
        write (number, '(i0)') value
        call write_text(join_path(root, 'fpm.toml'), &
            'name = "provider"'//new_line('a')//'version = "'//version//'"'// &
            new_line('a')//'[dependencies]'//new_line('a')// &
            'leaf = { namespace = "demo", v = "1.0.0" }'//new_line('a')// &
            '[dev-dependencies]'//new_line('a')// &
            'irrelevant = { namespace = "missing", v = "99.0.0" }'//new_line('a'))
        call write_text(join_path(root, 'src/provider.f90'), &
            'module provider'//new_line('a')//'use leaf, only: leaf_value'// &
            new_line('a')//'contains'//new_line('a')// &
            'integer function provider_value()'//new_line('a')// &
            'provider_value = '//trim(number)//' + leaf_value()'//new_line('a')// &
            'end function'//new_line('a')//'end module'//new_line('a'))
    end subroutine write_provider

    subroutine expect_output(expected, label)
        character(len=*), intent(in) :: expected, label
        args = string_list_t()
        call list_add(args, 'build')
        call run_fo(driver, args, consumer, join_path(scratch, 'cache'), result, &
            environment)
        call assert_process_ok(result, 'build '//label)
        args = string_list_t()
        call list_add(args, 'exec')
        call list_add(args, 'probe')
        call run_fo(driver, args, consumer, join_path(scratch, 'cache'), result, &
            environment)
        call assert_process_ok(result, 'exec '//label)
        call assert_equal_string(result%stdout, expected//new_line('a'), label)
    end subroutine expect_output

    subroutine expect_failure(diagnostic, label)
        character(len=*), intent(in) :: diagnostic, label
        args = string_list_t()
        call list_add(args, 'build')
        call run_fo(driver, args, consumer, join_path(scratch, 'cache'), result, &
            environment)
        call assert_true(result%exit_code /= 0, label)
        call assert_contains(result%stderr, diagnostic, label//' diagnostic')
    end subroutine expect_failure
    subroutine find_command(name, path)
        character(len=*), intent(in) :: name
        character(:), allocatable, intent(out) :: path
        character(len=4096) :: search_path
        character(len=512) :: candidate
        integer :: length, status, start, finish, i

        call get_environment_variable('PATH', search_path, length, status)
        call assert_true(status == 0 .and. length > 0, &
            'read process PATH while locating '//name)
        start = 1
        do i = 1, length + 1
            if (i <= length) then
                if (search_path(i:i) /= ':') cycle
            end if
            finish = i - 1
            if (finish < start) then
                start = i + 1
                cycle
            end if
            candidate = trim(search_path(start:finish))//'/'//trim(name)
            if (file_exists(trim(candidate))) then
                path = trim(candidate)
                return
            end if
            start = i + 1
        end do
        call assert_true(.false., 'cannot locate required tool '//name)
        path = ''
    end subroutine find_command
end program test_native_registry_cli
