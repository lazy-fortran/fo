program test_test_c_helpers_cli
    use fo_test_harness, only: process_result_t, string_list_t, list_add
    use fo_test_harness, only: make_scratch, join_path, write_text, write_lines
    use fo_test_harness, only: remove_path, assert_process_ok, assert_true
    use fo_test_harness, only: assert_file_equals, finish_assertions
    use fo_test_cli, only: resolve_driver, run_fo
    implicit none
    character(:), allocatable :: driver, scratch, consumer, cache
    type(process_result_t) :: result
    type(string_list_t) :: arguments, environment
    character(len=1), parameter :: nl = new_line('a')
    character(len=88) :: lines(13)

    call resolve_driver(driver)
    call make_scratch('fo-test-c-helpers', scratch)
    consumer = join_path(scratch, 'consumer')
    cache = join_path(scratch, 'cache')
    call list_add(environment, 'FO_JOBS=1')
    call write_text(join_path(consumer, 'fpm.toml'), &
        'name = "test_c_helpers"' // nl // &
        '[[test]]' // nl // 'name = "declared"' // nl // &
        'source-dir = "checks/custom"' // nl // 'main = "probe.f90"' // nl // &
        '[[test]]' // nl // 'name = "c_main"' // nl // &
        'source-dir = "test/first"' // nl // 'main = "main.c"' // nl)
    lines = [character(len=88) :: &
        'program probe', 'use iso_c_binding, only: c_int', 'implicit none', &
        'interface', 'integer(c_int) function helper() bind(C)', &
        'import c_int', 'end function', 'end interface', 'integer :: unit', &
        "open(newunit=unit, file='value.receipt', status='replace')", &
        "write(unit, '(i0)') helper()", 'close(unit)', 'end program probe']
    call write_lines(join_path(consumer, 'test/first/test_first.f90'), lines)
    call write_lines(join_path(consumer, 'test/second/test_second.f90'), lines)
    call write_lines(join_path(consumer, 'checks/custom/probe.f90'), lines)
    call write_text(join_path(consumer, 'test/first/nested/helper.c'), &
        '#include "value.h"' // nl // 'int helper(void) { return VALUE; }' // nl)
    call write_text(join_path(consumer, 'test/first/nested/value.h'), &
        '#define VALUE 21' // nl)
    call write_text(join_path(consumer, 'test/first/main.c'), &
        'int main(void) { return 99; }' // nl)
    call write_text(join_path(consumer, 'test/second/helper.cpp'), &
        '#ifndef __cplusplus' // nl // '#error C++ compiler required' // nl // &
        '#endif' // nl // 'extern "C" int helper(void) { return 42; }' // nl)
    call write_text(join_path(consumer, 'checks/custom/helper.c'), &
        'int helper(void) { return 63; }' // nl)
    call write_text(join_path(consumer, 'app/production.f90'), &
        'program production' // nl // 'end program production' // nl)

    call expect_value('test_first', '21', 'cold nested C helper')
    call expect_value('test_second', '42', 'C++ helper with colliding sibling symbol')
    call expect_value('declared', '63', 'explicit test root outside test directory')
    call expect_value('test_first', '21', 'warm C helper')
    call write_text(join_path(consumer, 'test/first/nested/value.h'), &
        '#define VALUE 24' // nl)
    call expect_value('test_first', '24', 'header edit invalidates cached test')
    call write_text(join_path(consumer, 'test/first/nested/helper.c'), &
        'int helper(void) { return 25; }' // nl)
    call expect_value('test_first', '25', 'source edit invalidates cached test')
    call remove_path(join_path(consumer, 'test/first/nested/helper.c'))
    call test_arguments('test_first')
    call run_fo(driver, arguments, consumer, cache, result, environment)
    call assert_true(result%exit_code /= 0, 'removed helper cannot reuse warm binary')
    call write_text(join_path(consumer, 'test/first/nested/helper.c'), &
        '#error Test source must not compile for a production target' // nl)
    arguments = string_list_t()
    call list_add(arguments, 'exec')
    call list_add(arguments, 'production')
    call run_fo(driver, arguments, consumer, cache, result, environment)
    call assert_process_ok(result, 'production excludes test C sources')
    call finish_assertions(retain_failed_scratch=.true.)
contains
    subroutine test_arguments(name)
        character(len=*), intent(in) :: name
        arguments = string_list_t()
        call list_add(arguments, 'test')
        call list_add(arguments, name)
    end subroutine test_arguments

    subroutine expect_value(name, expected, context)
        character(len=*), intent(in) :: name, expected, context
        call remove_path(join_path(consumer, 'value.receipt'))
        call test_arguments(name)
        call run_fo(driver, arguments, consumer, cache, result, environment)
        call assert_process_ok(result, context)
        call assert_file_equals(join_path(consumer, 'value.receipt'), &
            expected // nl, context)
    end subroutine expect_value
end program test_test_c_helpers_cli
