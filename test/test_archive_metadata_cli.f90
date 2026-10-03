program test_archive_metadata_cli
    use fo_test_harness, only: process_result_t, string_list_t, list_add
    use fo_test_harness, only: make_scratch, join_path, write_text, remove_tree
    use fo_test_harness, only: assert_process_ok, assert_equal_string, assert_true
    use fo_test_harness, only: finish_assertions
    use fo_test_cli, only: resolve_driver, run_fo, run_external
    implicit none

    character(:), allocatable :: driver, scratch, dependency, consumer
    character(:), allocatable :: tools, ar_path, wrapper
    type(process_result_t) :: result, lookup
    type(string_list_t) :: arguments, environment

    call resolve_driver(driver)
    call make_scratch('fo-archive-metadata', scratch)
    tools = join_path(scratch, 'tools')
    dependency = join_path(scratch, 'dependency')
    consumer = join_path(scratch, 'consumer')
    wrapper = join_path(tools, 'ar')

    call list_add(arguments, 'ar')
    call run_external('/usr/bin/which', arguments, scratch, lookup)
    call assert_process_ok(lookup, 'locate real archiver')
    ar_path = trim(lookup%stdout)
    if (len(ar_path) > 0) then
        if (ar_path(len(ar_path):len(ar_path)) == new_line('a')) &
            ar_path = ar_path(:len(ar_path) - 1)
    end if
    call write_text(wrapper, archiver_wrapper(ar_path))
    arguments = string_list_t()
    call list_add(arguments, '+x')
    call list_add(arguments, wrapper)
    call run_external('/bin/chmod', arguments, scratch, result)
    call assert_process_ok(result, 'make controlled archiver executable')
    arguments = string_list_t()

    call write_text(join_path(dependency, 'fpm.toml'), 'name = "archive_provider"' // new_line('a'))
    call write_text(join_path(dependency, 'src/provider.f90'), provider_source(8))
    call write_text(join_path(consumer, 'fpm.toml'), &
        'name = "archive_consumer"' // new_line('a') // '[dependencies]' // new_line('a') // &
        'archive_provider = { path = "../dependency" }' // new_line('a'))
    call write_text(join_path(consumer, 'app/probe.f90'), &
        'program probe' // new_line('a') // 'use provider, only: current_value' // new_line('a') // &
        'implicit none' // new_line('a') // "print '(i0)', current_value()" // new_line('a') // &
        'end program probe' // new_line('a'))
    call list_add(environment, 'PATH=' // tools // ':' // environment_value('PATH'))
    call list_add(environment, 'FO_ARCHIVE_MEMBER_MODE=apple-index')

    call expect_value(8, 'cold archive with Apple symbol index')
    call expect_value(8, 'warm cached archive with Apple symbol index')

    call expect_rejected('extra-object', 11, 'unexpected object member')
    call expect_rejected('extra-payload', 12, 'unexpected payload member')
    call expect_rejected('missing', 13, 'missing expected object')
    call expect_rejected('duplicate', 14, 'duplicate expected object')
    call expect_rejected('truncate', 15, 'truncated archive')

    call remove_tree(scratch)
    call finish_assertions()
    write(*, '(a)') 'archive-metadata-cli: Apple index accepted; invalid member sets rejected'

contains

    function archiver_wrapper(real_ar) result(script)
        character(len=*), intent(in) :: real_ar
        character(:), allocatable :: script

        script = '#!/bin/sh' // new_line('a') // &
            'REAL_AR="' // trim(real_ar) // '"' // new_line('a') // &
            'if [ "$1" = "t" ]; then' // new_line('a') // &
            '  listing="$2.listing.$$"' // new_line('a') // &
            '  "$REAL_AR" "$@" > "$listing" || exit $?' // new_line('a') // &
            '  case "$FO_ARCHIVE_MEMBER_MODE" in' // new_line('a') // &
            '    missing) [ ! -f "$listing" ] || grep "^__.SYMDEF" "$listing"; rm -f "$listing"; exit 0 ;;' // new_line('a') // &
            '  esac' // new_line('a') // &
            '  cat "$listing"' // new_line('a') // &
            '  case "$FO_ARCHIVE_MEMBER_MODE" in' // new_line('a') // &
            '    apple-index)' // new_line('a') // &
            '      if [ "$(uname -s)" = "Darwin" ]; then' // new_line('a') // &
            '        grep -Fqx "__.SYMDEF SORTED" "$listing" || exit 73' // new_line('a') // &
            '      else' // new_line('a') // &
            '        grep -Fqx "__.SYMDEF SORTED" "$listing" || echo "__.SYMDEF SORTED"' // new_line('a') // &
            '      fi ;;' // new_line('a') // &
            '    extra-object) echo "injected-extra.o" ;;' // new_line('a') // &
            '    extra-payload) echo "injected-payload.dat" ;;' // new_line('a') // &
            '    duplicate) awk "NF && \\$0 !~ /^__\\.SYMDEF/ { print; print; exit }" "$listing" ;;' // new_line('a') // &
            '  esac' // new_line('a') // &
            '  rm -f "$listing"' // new_line('a') // &
            '  exit 0' // new_line('a') // &
            'fi' // new_line('a') // &
            'if [ "$1" = "rcs" ] && [ "$FO_ARCHIVE_MEMBER_MODE" = "truncate" ]; then' // new_line('a') // &
            '  "$REAL_AR" "$@" || exit $?' // new_line('a') // &
            '  printf "!<arch>\\n" > "$3"' // new_line('a') // &
            '  exit 0' // new_line('a') // &
            'fi' // new_line('a') // &
            'exec "$REAL_AR" "$@"' // new_line('a')
    end function archiver_wrapper

    function provider_source(value) result(text)
        integer, intent(in) :: value
        character(:), allocatable :: text
        character(len=32) :: digits

        write(digits, '(i0)') value
        text = 'module provider' // new_line('a') // 'implicit none' // new_line('a') // &
            'contains' // new_line('a') // 'integer function current_value()' // new_line('a') // &
            'current_value = ' // trim(digits) // new_line('a') // &
            'end function current_value' // new_line('a') // 'end module provider' // new_line('a')
    end function provider_source

    subroutine expect_value(value, context)
        integer, intent(in) :: value
        character(len=*), intent(in) :: context
        character(len=32) :: digits

        write(digits, '(i0)') value
        call list_add(arguments, 'exec')
        call list_add(arguments, 'probe')
        call run_fo(driver, arguments, consumer, join_path(scratch, 'cache'), result, environment)
        if (result%exit_code /= 0) then
            write(*, '(a)') 'fo stdout: ' // result%stdout
            write(*, '(a)') 'fo stderr: ' // result%stderr
        end if
        call assert_process_ok(result, context)
        call assert_equal_string(result%stdout, trim(digits) // new_line('a'), &
            context // ': linked program returns expected value')
        arguments = string_list_t()
    end subroutine expect_value

    subroutine expect_rejected(mode, value, context)
        character(len=*), intent(in) :: mode, context
        integer, intent(in) :: value

        call write_text(join_path(dependency, 'src/provider.f90'), provider_source(value))
        environment = string_list_t()
        call list_add(environment, 'PATH=' // tools // ':' // environment_value('PATH'))
        call list_add(environment, 'FO_ARCHIVE_MEMBER_MODE=' // mode)
        call list_add(arguments, 'exec')
        call list_add(arguments, 'probe')
        call run_fo(driver, arguments, consumer, join_path(scratch, 'cache'), result, environment)
        if (result%exit_code == 0) then
            write(*, '(a)') 'unexpected fo stdout: ' // result%stdout
        end if
        call assert_true(result%exit_code /= 0, context // ': archive is rejected')
        arguments = string_list_t()
    end subroutine expect_rejected

    function environment_value(name) result(value)
        character(len=*), intent(in) :: name
        character(:), allocatable :: value
        integer :: length, status

        call get_environment_variable(name, length=length, status=status)
        if (status /= 0 .or. length < 1) then
            value = ''
            return
        end if
        allocate(character(len=length) :: value)
        call get_environment_variable(name, value=value, status=status)
        if (status /= 0) value = ''
    end function environment_value

end program test_archive_metadata_cli
