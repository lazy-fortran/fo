program test_archive_metadata_cli
    use fo_test_harness, only: process_result_t, string_list_t, list_add
    use fo_test_harness, only: make_scratch, join_path, write_text, remove_tree
    use fo_test_harness, only: assert_process_ok, assert_equal_string, assert_true
    use fo_test_harness, only: assert_contains
    use fo_test_harness, only: finish_assertions
    use fo_test_cli, only: resolve_driver, run_fo, run_external
    implicit none

    character(:), allocatable :: driver, scratch, dependency, consumer
    character(:), allocatable :: tools, ar_path, wrapper, platform, native_path
    type(process_result_t) :: result, lookup
    type(string_list_t) :: arguments, environment
    type(string_list_t) :: native_environment

    call resolve_driver(driver)
    call make_scratch('fo-archive-metadata', scratch)
    tools = join_path(scratch, 'tools')
    dependency = join_path(scratch, 'dependency')
    consumer = join_path(scratch, 'consumer')
    wrapper = join_path(tools, 'ar')

    native_path = '/usr/bin:/bin:/opt/homebrew/bin'
    call list_add(arguments, '-s')
    call run_external('/usr/bin/uname', arguments, scratch, lookup)
    call assert_process_ok(lookup, 'identify host platform')
    platform = first_line(lookup%stdout)
    arguments = string_list_t()
    call list_add(native_environment, 'PATH=' // native_path)
    call list_add(arguments, 'ar')
    call run_external('/usr/bin/which', arguments, scratch, lookup, native_environment)
    call assert_process_ok(lookup, 'locate real archiver')
    ar_path = first_line(lookup%stdout)
    if (platform == 'Darwin') call assert_equal_string(ar_path, '/usr/bin/ar', &
        'Darwin native archiver is Apple cctools /usr/bin/ar')
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
    call list_add(environment, 'PATH=' // native_path)

    call expect_value(8, 'native-cache', 'cold archive from the native archiver')
    if (platform == 'Darwin') then
        call verify_native_index(scratch, ar_path)
        write(*, '(a)') 'native Apple ar: ' // ar_path // ' reports __.SYMDEF SORTED'
    end if
    call expect_value(8, 'native-cache', 'warm archive from the native archiver')

    environment = string_list_t()
    call list_add(environment, 'PATH=' // tools // ':' // native_path)
    call list_add(environment, 'FO_ARCHIVE_MEMBER_MODE=apple-index')
    call expect_value(8, 'controlled-cache', 'cold archive with controlled Apple symbol index')
    call expect_value(8, 'controlled-cache', 'warm archive with controlled Apple symbol index')

    call expect_rejected('extra-object', 11, 'unexpected object member')
    call expect_rejected('extra-payload', 12, 'unexpected payload member')
    call expect_rejected('missing', 13, 'missing expected object')
    environment = string_list_t()
    call list_add(environment, 'PATH=' // tools // ':' // native_path)
    call list_add(environment, 'FO_ARCHIVE_MEMBER_MODE=apple-index')
    call write_text(join_path(dependency, 'src/provider.f90'), provider_source(14))
    call expect_value(14, 'controlled-cache', 'build archive for duplicate-listing preflight')
    call verify_duplicate_listing(scratch)
    call expect_rejected('duplicate', 14, 'duplicate expected object')
    call expect_rejected('truncate', 15, 'truncated archive')

    call remove_tree(scratch)
    call finish_assertions()
    write(*, '(a)') 'archive-metadata-cli: native/controlled index accepted; invalid members rejected'

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
            '    missing)' // new_line('a') // &
            '      filtered="$listing.filtered"' // new_line('a') // &
            '      if grep "^__.SYMDEF" "$listing" > "$filtered"; then' // new_line('a') // &
            '        mv "$filtered" "$listing" || exit $?' // new_line('a') // &
            '      else' // new_line('a') // &
            '        status=$?' // new_line('a') // &
            '        [ "$status" -eq 1 ] || { rm -f "$listing" "$filtered"; exit "$status"; }' // new_line('a') // &
            '        : > "$filtered" || exit $?' // new_line('a') // &
            '        mv "$filtered" "$listing" || exit $?' // new_line('a') // &
            '      fi ;;' // new_line('a') // &
            '    duplicate)' // new_line('a') // &
            '      duplicate="$listing.duplicate"' // new_line('a') // &
            '      awk ''NF && $0 !~ /^__\.SYMDEF/ { print; print; exit }'' "$listing" > "$duplicate" || { rm -f "$listing" "$duplicate"; exit 74; }' // new_line('a') // &
            '      first=$(sed -n "1p" "$duplicate") || exit $?' // new_line('a') // &
            '      second=$(sed -n "2p" "$duplicate") || exit $?' // new_line('a') // &
            '      lines=$(wc -l < "$duplicate") || exit $?' // new_line('a') // &
            '      if [ "$lines" -ne 2 ] || [ -z "$first" ] || [ "$first" != "$second" ]; then' // new_line('a') // &
            '        rm -f "$listing" "$duplicate"; exit 75' // new_line('a') // &
            '      fi' // new_line('a') // &
            '      sed -n "1p" "$duplicate" >> "$listing" || exit $?' // new_line('a') // &
            '      rm -f "$duplicate" || exit $?' // new_line('a') // &
            '      ;;' // new_line('a') // &
            '  esac' // new_line('a') // &
            '  cat "$listing" || { status=$?; rm -f "$listing"; exit "$status"; }' // new_line('a') // &
            '  case "$FO_ARCHIVE_MEMBER_MODE" in' // new_line('a') // &
            '    apple-index)' // new_line('a') // &
            '      if [ "$(uname -s)" = "Darwin" ]; then' // new_line('a') // &
            '        grep -Fqx "__.SYMDEF SORTED" "$listing" || { rm -f "$listing"; exit 73; }' // new_line('a') // &
            '      else' // new_line('a') // &
            '        grep -Fqx "__.SYMDEF SORTED" "$listing" || echo "__.SYMDEF SORTED" || exit $?' // new_line('a') // &
            '      fi ;;' // new_line('a') // &
            '    extra-object) echo "injected-extra.o" || exit $? ;;' // new_line('a') // &
            '    extra-payload) echo "injected-payload.dat" || exit $? ;;' // new_line('a') // &
            '  esac' // new_line('a') // &
            '  rm -f "$listing" || exit $?' // new_line('a') // &
            '  exit 0' // new_line('a') // &
            'fi' // new_line('a') // &
            'if [ "$1" = "rcs" ] && [ "$FO_ARCHIVE_MEMBER_MODE" = "truncate" ]; then' // new_line('a') // &
            '  "$REAL_AR" "$@" || exit $?' // new_line('a') // &
            '  printf "!<arch>\\n" > "$3" || exit $?' // new_line('a') // &
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

    subroutine expect_value(value, cache_name, context)
        integer, intent(in) :: value
        character(len=*), intent(in) :: cache_name, context
        character(len=32) :: digits

        write(digits, '(i0)') value
        call list_add(arguments, 'exec')
        call list_add(arguments, 'probe')
        call run_fo(driver, arguments, consumer, join_path(scratch, cache_name), result, environment)
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
        call list_add(environment, 'PATH=' // tools // ':' // native_path)
        call list_add(environment, 'FO_ARCHIVE_MEMBER_MODE=' // mode)
        call list_add(arguments, 'exec')
        call list_add(arguments, 'probe')
        call run_fo(driver, arguments, consumer, join_path(scratch, 'controlled-cache'), &
            result, environment)
        if (result%exit_code == 0) then
            write(*, '(a)') 'unexpected fo stdout: ' // result%stdout
        end if
        call assert_true(result%exit_code /= 0, context // ': archive is rejected')
        arguments = string_list_t()
    end subroutine expect_rejected

    subroutine verify_duplicate_listing(root)
        character(len=*), intent(in) :: root
        character(:), allocatable :: archive_path
        type(process_result_t) :: child

        arguments = string_list_t()
        call list_add(arguments, root)
        call list_add(arguments, '-type')
        call list_add(arguments, 'f')
        call list_add(arguments, '-name')
        call list_add(arguments, '*.a')
        call run_external('/usr/bin/find', arguments, root, child)
        call assert_process_ok(child, 'find archive for duplicate-listing preflight')
        archive_path = first_line(child%stdout)
        call assert_true(len_trim(archive_path) > 0, 'archive exists before duplicate preflight')

        arguments = string_list_t()
        call list_add(arguments, 't')
        call list_add(arguments, archive_path)
        environment = string_list_t()
        call list_add(environment, 'PATH=' // tools // ':' // native_path)
        call list_add(environment, 'FO_ARCHIVE_MEMBER_MODE=duplicate')
        call run_external('ar', arguments, consumer, child, environment)
        call assert_process_ok(child, 'duplicate mutation emits a successful archive listing')
        call assert_true(has_duplicate_object(child%stdout), &
            'controlled duplicate listing contains two identical ordinary members')
        arguments = string_list_t()
    end subroutine verify_duplicate_listing

    logical function has_duplicate_object(listing) result(found)
        character(len=*), intent(in) :: listing
        character(len=1024) :: seen(128)
        character(:), allocatable :: member
        integer :: position, newline_position, n_seen, i

        found = .false.
        seen = ''
        n_seen = 0
        position = 1
        do while (position <= len(listing))
            newline_position = index(listing(position:), new_line('a'))
            if (newline_position > 0) then
                if (newline_position > 1) then
                    member = trim(listing(position:position + newline_position - 2))
                else
                    member = ''
                end if
                position = position + newline_position
            else
                member = trim(listing(position:))
                position = len(listing) + 1
            end if
            if (len(member) == 0 .or. index(member, '__.SYMDEF') == 1) cycle
            do i = 1, n_seen
                if (trim(seen(i)) == member) then
                    found = .true.
                    return
                end if
            end do
            if (n_seen < size(seen)) then
                n_seen = n_seen + 1
                seen(n_seen) = member
            end if
        end do
    end function has_duplicate_object

    subroutine verify_native_index(root, real_ar)
        character(len=*), intent(in) :: root, real_ar
        character(:), allocatable :: archive_path
        type(process_result_t) :: child

        arguments = string_list_t()
        call list_add(arguments, root)
        call list_add(arguments, '-type')
        call list_add(arguments, 'f')
        call list_add(arguments, '-name')
        call list_add(arguments, '*.a')
        call run_external('/usr/bin/find', arguments, root, child)
        call assert_process_ok(child, 'find archive built by native archiver')
        archive_path = first_line(child%stdout)
        call assert_true(len_trim(archive_path) > 0, 'native archive exists for index inspection')

        arguments = string_list_t()
        call list_add(arguments, '-t')
        call list_add(arguments, archive_path)
        call run_external(real_ar, arguments, root, child)
        call assert_process_ok(child, 'list members with native Apple ar')
        call assert_contains(child%stdout, '__.SYMDEF SORTED', &
            'real Apple archive contains its native sorted symbol index')
        arguments = string_list_t()
    end subroutine verify_native_index

    function first_line(text) result(line)
        character(len=*), intent(in) :: text
        character(:), allocatable :: line
        integer :: newline_position

        newline_position = index(text, new_line('a'))
        if (newline_position > 0) then
            line = text(:newline_position - 1)
        else
            line = trim(text)
        end if
    end function first_line

end program test_archive_metadata_cli
