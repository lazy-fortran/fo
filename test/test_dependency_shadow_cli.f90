program test_dependency_shadow_cli
    use fo_test_harness, only: process_result_t, string_list_t, list_add
    use fo_test_harness, only: make_scratch, join_path, write_text, write_lines, remove_tree
    use fo_test_harness, only: assert_file_equals, assert_process_ok
    use fo_test_cli, only: resolve_driver, run_fo, run_external
    use fo_test_harness, only: finish_assertions
    implicit none

    character(:), allocatable :: driver, scratch, external, shadow, fx, consumer, git_dependency
    type(process_result_t) :: result
    type(string_list_t) :: arguments, environment

    call resolve_driver(driver)
    call make_scratch('fo-dependency-shadow', scratch)
    external = join_path(scratch, 'fortfront')
    shadow = join_path(scratch, 'fortfront-path-copy')
    fx = join_path(scratch, 'fx')
    consumer = join_path(scratch, 'consumer')
    call list_add(environment, 'FO_JOBS=2')

    call write_text(join_path(external, 'fpm.toml'), &
        'name = "fortfront_probe"' // new_line('a') // 'version = "0.1.0"' // new_line('a'))
    call write_lines(join_path(external, 'src/fortfront_probe.f90'), [character(len=256) :: &
        'module fortfront_probe', 'contains', &
        'integer function fortfront_value()', 'fortfront_value = 17', &
        'end function fortfront_value', 'end module fortfront_probe'])
    call git_invoke(external, [character(len=64) :: 'init', '-q', '-b', 'main'])
    call git_invoke(external, [character(len=64) :: 'add', 'fpm.toml', &
        'src/fortfront_probe.f90'])
    arguments = string_list_t()
    call list_add(arguments, '-c')
    call list_add(arguments, 'user.name=Probe')
    call list_add(arguments, '-c')
    call list_add(arguments, 'user.email=probe@example.invalid')
    call list_add(arguments, 'commit')
    call list_add(arguments, '-q')
    call list_add(arguments, '-m')
    call list_add(arguments, 'fixture')
    call invoke('git', arguments, external)

    call write_text(join_path(shadow, 'fpm.toml'), &
        'name = "fortfront_probe"' // new_line('a') // 'version = "0.1.0"' // new_line('a'))
    call write_lines(join_path(shadow, 'src/fortfront_probe.f90'), [character(len=256) :: &
        'module fortfront_probe', 'contains', &
        'integer function fortfront_value()', 'fortfront_value = 99', &
        'end function fortfront_value', 'end module fortfront_probe'])
    call write_text(join_path(fx, 'fpm.toml'), &
        'name = "fx_probe"' // new_line('a') // 'version = "0.1.0"' // new_line('a') // &
        '[dependencies]' // new_line('a') // &
        'fortfront_probe = { path = "../fortfront-path-copy" }' // new_line('a'))
    call write_lines(join_path(fx, 'src/fx_probe.f90'), [character(len=256) :: &
        'module fx_probe', 'use fortfront_probe, only: fortfront_value', 'contains', &
        'integer function fx_value()', 'fx_value = fortfront_value()', &
        'end function fx_value', 'end module fx_probe'])
    git_dependency = 'fortfront_probe = { git = "file://' // external // &
        '", branch = "main" }'
    call write_text(join_path(consumer, 'fpm.toml'), &
        'name = "dependency_shadow_probe"' // new_line('a') // 'version = "0.1.0"' // &
        new_line('a') // '[dependencies]' // new_line('a') // &
        'fx_probe = { path = "../fx" }' // new_line('a') // git_dependency // new_line('a'))
    call write_lines(join_path(consumer, 'src/consumer.f90'), [character(len=256) :: &
        'module consumer', 'use fx_probe, only: fx_value', 'contains', &
        'integer function consumer_value()', 'consumer_value = fx_value()', &
        'end function consumer_value', 'end module consumer'])
    call write_lines(join_path(consumer, 'test/test_probe.f90'), [character(len=256) :: &
        'program test_probe', 'use consumer, only: consumer_value', 'implicit none', &
        'integer :: unit', &
        "open(newunit=unit, file='consumer.receipt', status='replace')", &
        "write(unit, '(i0)') consumer_value()", 'close(unit)', 'end program test_probe'])

    arguments = string_list_t()
    call list_add(arguments, 'build')
    call run_fo(driver, arguments, consumer, join_path(scratch, 'fo-cache'), result, environment)
    call assert_process_ok(result, 'dependency shadow build')
    arguments = string_list_t()
    call list_add(arguments, 'test')
    call list_add(arguments, 'test_probe')
    call run_fo(driver, arguments, consumer, join_path(scratch, 'fo-cache'), result, environment)
    call assert_process_ok(result, 'dependency shadow named test')
    call assert_file_equals(join_path(consumer, 'consumer.receipt'), '17' // new_line('a'), &
        'root Git provider wins over its transitive path copy')

    call remove_tree(scratch)
    call finish_assertions()
    write(*, '(a)') 'dependency-shadow-cli: root Git implementation wins over path copy'

contains

    subroutine git_invoke(cwd, words)
        character(len=*), intent(in) :: cwd
        character(len=*), intent(in) :: words(:)
        type(string_list_t) :: args
        integer :: i

        do i = 1, size(words)
            call list_add(args, trim(words(i)))
        end do
        call invoke('git', args, cwd)
    end subroutine git_invoke

    subroutine invoke(program, args, cwd)
        character(len=*), intent(in) :: program, cwd
        type(string_list_t), intent(in) :: args

        call run_external(program, args, cwd, result)
        call assert_process_ok(result, program // ' fixture command')
    end subroutine invoke

end program test_dependency_shadow_cli
