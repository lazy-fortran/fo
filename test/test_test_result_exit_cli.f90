program test_test_result_exit_cli
    use fo_test_harness, only: process_result_t, string_list_t, list_add, &
                     make_scratch, join_path, write_text, make_directory, run_process, &
                 assert_true, assert_equal_integer, assert_process_ok, finish_assertions
    use fo_fs, only: fs_find_executable
    use fo_test_cli, only: resolve_driver, run_fo, parse_json_report
    use fo_test_json, only: json_value_t, json_member, json_number_value
    implicit none
    character(:), allocatable :: driver, scratch
    character(len=16384) :: original_path
    character(len=4096) :: real_ctest
    logical :: found
    type(string_list_t) :: arguments, environment, cc_args
    type(process_result_t) :: observed
    type(json_value_t) :: report, field
    integer :: status

    call resolve_driver(driver)
    call fs_find_executable('ctest', real_ctest, found)
    if (.not. found) error stop 'test result oracle requires CTest'
    call make_scratch('fo-test-result-exit', scratch)
    call make_directory(join_path(scratch, 'bin'))
    call get_environment_variable('PATH', original_path, status=status)
    call assert_equal_integer(status, 0, 'fixture obtains PATH')
    call write_text(join_path(scratch, 'CMakeLists.txt'), &
                    'cmake_minimum_required(VERSION 3.22)'//new_line('a')// &
                    'project(result_exit NONE)'//new_line('a')// &
                    'enable_testing()'//new_line('a')// &
                    'add_test(NAME selected COMMAND "${CMAKE_COMMAND}" -E true)')
    call write_text(join_path(scratch, 'fake_ctest.c'), &
                    '#include <stdio.h>'//new_line('a')// &
                    '#include <stdlib.h>'//new_line('a')// &
                    '#include <string.h>'//new_line('a')// &
                    '#include <unistd.h>'//new_line('a')// &
                    'int main(int argc, char **argv) {'//new_line('a')// &
               ' const char *mode = getenv("FO_FIXTURE_CTEST_MODE");'//new_line('a')// &
                    ' (void)argc;'//new_line('a')// &
                    ' if (mode && !strcmp(mode,"malformed")) {'//new_line('a')// &
                  '  puts("1/1 Test #1: selected ........ UnknownStatus 0.00 sec");'// &
                    new_line('a')//'  return 0;'//new_line('a')//' }'//new_line('a')// &
                    ' if (mode && !strcmp(mode,"empty")) return 0;'//new_line('a')// &
                    ' const char *real = getenv("FO_FIXTURE_REAL_CTEST");'// &
                    new_line('a')//' if (!real) return 127;'//new_line('a')// &
                    ' execv(real,argv); return 127;'//new_line('a')//'}')
    call list_add(cc_args, 'cc')
    call list_add(cc_args, join_path(scratch, 'fake_ctest.c'))
    call list_add(cc_args, '-o')
    call list_add(cc_args, join_path(scratch, 'bin/ctest'))
    call run_process(cc_args, scratch, observed)
    call assert_process_ok(observed, 'independent CTest response shim compiles')
    call list_add(environment, 'FO_BACKEND=cmake')
    call list_add(environment, &
                  'PATH='//join_path(scratch, 'bin')//':'//trim(original_path))
    call list_add(environment, 'FO_FIXTURE_CTEST_MODE=malformed')
    call list_add(environment, 'FO_FIXTURE_REAL_CTEST='//trim(real_ctest))
    call list_add(arguments, 'test')
    call list_add(arguments, 'selected')
    call list_add(arguments, '--json')
    call run_fo(driver, arguments, scratch, join_path(scratch, 'cache'), &
                observed, environment)
    call assert_true(observed%exit_code /= 0, 'malformed verdict makes public CLI fail')
    call parse_json_report(observed, report, 'malformed verdict still has a JSON error')
    field = json_member(report, 'exit_code')
    call assert_equal_integer(int(json_number_value(field)), 1, &
                              'JSON agrees with failed process')
    environment%items(3)%value = 'FO_FIXTURE_CTEST_MODE=empty'
    call run_fo(driver, arguments, scratch, join_path(scratch, 'cache'), &
                observed, environment)
    call assert_true(observed%exit_code /= 0, &
                     'selected test cannot pass with no verdict')
    call parse_json_report(observed, report, 'empty selected result reports JSON error')
    environment%items(3)%value = 'FO_FIXTURE_CTEST_MODE=real'
    call run_fo(driver, arguments, scratch, join_path(scratch, 'cache'), &
                observed, environment)
    call assert_process_ok(observed, 'actual registered CTest verdict passes')
    call parse_json_report(observed, report, 'actual registered verdict retains JSON')
    call write_text(join_path(scratch, 'CMakeLists.txt'), &
                    'cmake_minimum_required(VERSION 3.22)'//new_line('a')// &
                    'project(result_exit LANGUAGES Fortran)'//new_line('a')// &
                    'enable_testing()'//new_line('a')// &
                    'add_executable(broken broken.f90)'//new_line('a')// &
                    'add_test(NAME selected COMMAND broken)')
    call write_text(join_path(scratch, 'broken.f90'), 'this is invalid Fortran')
    call run_fo(driver, arguments, scratch, join_path(scratch, 'cache'), &
                observed, environment)
    call assert_true(observed%exit_code /= 0, 'failed native compile remains failure')
    call parse_json_report(observed, report, 'failed compile retains structured JSON')
    field = json_member(report, 'exit_code')
    call assert_true(int(json_number_value(field)) /= 0, &
                     'failed compile JSON preserves nonzero verdict')
    call finish_assertions()
end program test_test_result_exit_cli
