program test_dev_dependency_c_cli
    use fo_test_harness, only: process_result_t, string_list_t, list_add
    use fo_test_harness, only: make_scratch, join_path, write_text, write_lines
    use fo_test_harness, only: remove_tree, remove_path, assert_process_ok
    use fo_test_harness, only: assert_equal_string, assert_file_equals, assert_true
    use fo_test_harness, only: assert_contains, assert_not_contains, finish_assertions
    use fo_test_cli, only: resolve_driver, run_fo, run_external
    implicit none

    character(:), allocatable :: driver, scratch, consumer, dependency, cache
    type(process_result_t) :: result
    type(string_list_t) :: arguments, environment
    character(len=88) :: test_lines(8), production_lines(4)
    character(len=1), parameter :: nl = new_line('a')

    call resolve_driver(driver)
    call make_scratch('fo-dev-dependency-c', scratch)
    consumer = join_path(scratch, 'consumer')
    dependency = join_path(consumer, 'test-support/dev')
    cache = join_path(scratch, 'empty-cache')
    call list_add(environment, 'FO_JOBS=1')
    call list_add(environment, 'FO_DEBUG_LINKS=1')
    call write_text(join_path(consumer, 'fpm.toml'), &
        'name = "dev_consumer"' // nl // '[dev-dependencies]' // nl // &
        'devhelper = { path = "test-support/dev" }' // nl)
    call write_text(join_path(dependency, 'fpm.toml'), 'name = "devhelper"' // nl)
    call write_text(join_path(dependency, 'include/value.h'), '#define DEV_VALUE 21' // nl)
    call write_shim(.false.)
    call write_helper(0)
    test_lines = [character(len=88) :: &
        'program test_probe', 'use devhelper, only: current_value', 'implicit none', &
        'integer :: unit', "open(newunit=unit, file='dev.receipt', status='replace')", &
        "write(unit, '(i0)') current_value()", 'close(unit)', 'end program test_probe']
    call write_lines(join_path(consumer, 'test/test_probe.f90'), test_lines)
    production_lines = [character(len=88) :: &
        'program production', 'implicit none', "print '(a)', 'production'", &
        'end program production']
    call write_lines(join_path(consumer, 'app/production.f90'), production_lines)

    ! The first operation is a test with no build tree, external profile or cache.
    call expect_value('21', '', 'cold static dev dependency')
    call assert_contains(result%stderr, 'test-support_dev_src_devshim.c.o', &
        'cold link explicitly includes the C shim')
    call assert_archive_members(result%stderr)
    call expect_value('21', '', 'warm static dev dependency')

    call write_text(join_path(dependency, 'include/value.h'), '#define DEV_VALUE 27' // nl)
    call expect_value('27', '', 'C header change invalidates the test link')
    call write_helper(4)
    call expect_value('31', '', 'Fortran implementation change invalidates the test link')
    call expect_value('36', '-DDEV_OFFSET=5', 'test flags change the dev module behavior')
    call expect_value('31', '', 'default flags restore the original behavior')

    call write_text(join_path(dependency, 'src/member.c'), &
        'int dev_member(void) { return 9; }' // nl)
    call write_shim(.true.)
    call expect_value('40', '', 'new C source joins the test link')
    call assert_contains(result%stderr, 'test-support_dev_src_member.c.o', &
        'new C source is an explicit link member')
    call remove_path(join_path(dependency, 'src/member.c'))
    call write_shim(.false.)
    call expect_value('31', '', 'removed C source leaves the test link')
    call assert_not_contains(result%stderr, 'test-support_dev_src_member.c.o', &
        'stale C object is excluded from the current link')

    ! A missing required provider must fail even after a successful warm build.
    call remove_path(join_path(dependency, 'src/devshim.c'))
    call test_arguments('')
    call run_fo(driver, arguments, consumer, cache, result, environment)
    call assert_true(result%exit_code /= 0, 'missing C provider cannot reuse a warm link')
    call write_shim(.false.)
    call expect_value('31', '', 'restored C provider repairs the link')

    call arguments_for('exec', 'production')
    call run_fo(driver, arguments, consumer, cache, result, environment)
    call assert_process_ok(result, 'production builds after dev dependency tests')
    call assert_equal_string(result%stdout, 'production' // nl, 'production behavior')
    call assert_not_contains(result%stderr, 'test-support_dev_src_', &
        'production link excludes all dev dependency objects')
    call assert_not_contains(result%stderr, 'objects_', &
        'standalone production link has no dev dependency archive')
    call symbols(join_path(consumer, 'build/fo/bin/production'))
    call assert_not_contains(result%stdout, 'dev_shim', 'production has no C test symbols')
    call assert_not_contains(result%stdout, 'devhelper', 'production has no Fortran test symbols')
    call symbols(join_path(consumer, 'build/fo/bin/test_probe'))
    call assert_contains(result%stdout, 'dev_shim', 'test binary contains the C provider')
    call assert_contains(result%stdout, 'devhelper', 'test binary contains the Fortran provider')

    call remove_tree(scratch)
    call finish_assertions()
    write (*, '(a)') 'dev-dependency-c-cli: cold closure, invalidation and production isolation'

contains

    subroutine write_shim(with_member)
        logical, intent(in) :: with_member
        character(:), allocatable :: source

        source = '#include "value.h"' // nl
        if (with_member) source = source // 'extern int dev_member(void);' // nl
        source = source // 'int dev_shim(void) { return DEV_VALUE'
        if (with_member) source = source // ' + dev_member()'
        call write_text(join_path(dependency, 'src/devshim.c'), source // '; }' // nl)
    end subroutine write_shim

    subroutine write_helper(offset)
        integer, intent(in) :: offset
        character(len=16) :: text
        character(len=88) :: lines(16)

        write (text, '(i0)') offset
        lines = [character(len=88) :: &
            'module devhelper', 'use iso_c_binding, only: c_int', 'implicit none', &
            'interface', 'integer(c_int) function dev_shim() bind(C)', 'import c_int', &
            'end function dev_shim', 'end interface', '#ifndef DEV_OFFSET', &
            '#define DEV_OFFSET 0', '#endif', 'contains', 'integer function current_value()', &
            'current_value = dev_shim() + DEV_OFFSET', &
            'end function current_value', 'end module devhelper']
        lines(14) = 'current_value = dev_shim() + ' // trim(text) // ' + DEV_OFFSET'
        call write_lines(join_path(dependency, 'src/devhelper.f90'), lines)
    end subroutine write_helper

    subroutine arguments_for(command, target)
        character(len=*), intent(in) :: command, target

        arguments = string_list_t()
        call list_add(arguments, command)
        call list_add(arguments, target)
    end subroutine arguments_for

    subroutine test_arguments(flags)
        character(len=*), intent(in) :: flags

        call arguments_for('test', 'test_probe')
        if (len_trim(flags) == 0) return
        call list_add(arguments, '--flag')
        call list_add(arguments, flags)
    end subroutine test_arguments

    subroutine expect_value(expected, flags, context)
        character(len=*), intent(in) :: expected, flags, context

        call remove_path(join_path(consumer, 'dev.receipt'))
        call test_arguments(flags)
        call run_fo(driver, arguments, consumer, cache, result, environment)
        call assert_process_ok(result, context)
        call assert_file_equals(join_path(consumer, 'dev.receipt'), expected // nl, context)
    end subroutine expect_value

    subroutine assert_archive_members(link_log)
        character(len=*), intent(in) :: link_log
        character(:), allocatable :: archive
        type(process_result_t) :: inventory
        type(string_list_t) :: ar_arguments
        integer :: last, first

        last = index(link_log, '.a ')
        call assert_true(last > 0, 'cold link has a static Fortran helper archive')
        if (last == 0) return
        first = last
        do while (first > 1)
            if (link_log(first - 1:first - 1) == ' ') exit
            first = first - 1
        end do
        archive = link_log(first:last + 1)
        call list_add(ar_arguments, 't')
        call list_add(ar_arguments, archive)
        call run_external('ar', ar_arguments, consumer, inventory)
        call assert_process_ok(inventory, 'read cold static archive inventory')
        ! Apple ar also lists its symbol table member, __.SYMDEF SORTED.
        call assert_equal_string(object_members(inventory%stdout), &
            'test-support_dev_src_devhelper.f90.o' // nl, &
            'cold archive contains exactly the selected Fortran helper')
    end subroutine assert_archive_members

    subroutine symbols(binary)
        character(len=*), intent(in) :: binary

        arguments = string_list_t()
        call list_add(arguments, '-g')
        call list_add(arguments, binary)
        call run_external('nm', arguments, consumer, result)
        call assert_process_ok(result, 'inspect binary symbols')
    end subroutine symbols

    function object_members(listing) result(members)
        character(len=*), intent(in) :: listing
        character(:), allocatable :: members
        integer :: first, last

        members = ''
        first = 1
        do while (first <= len(listing))
            last = index(listing(first:), nl)
            if (last == 0) then
                last = len(listing)
            else
                last = first + last - 2
            end if
            if (last >= first) then
                if (index(listing(first:last), '__.SYMDEF') /= 1) &
                    members = members // listing(first:last) // nl
            end if
            first = last + 2
        end do
    end function object_members

end program test_dev_dependency_c_cli
