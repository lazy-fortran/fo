program test_cmake_named_cli
    use fo_test_harness, only: process_result_t, string_list_t, list_add
    use fo_test_harness, only: make_scratch, join_path, current_directory
    use fo_test_harness, only: write_lines, remove_path, remove_tree
    use fo_test_harness, only: assert_equal_integer, assert_equal_string
    use fo_test_harness, only: assert_file_exists, assert_file_absent
    use fo_test_harness, only: assert_process_ok, assert_true
    use fo_test_cli, only: resolve_driver, run_fo, parse_json_report, exercise_harness
    use fo_test_json, only: json_value_t, json_member, json_element, json_size
    use fo_test_json, only: json_string_value
    implicit none

    character(:), allocatable :: driver, project, scratch, selected, neighbor
    character(:), allocatable :: selected_receipt, neighbor_receipt
    type(string_list_t) :: arguments, environment
    type(process_result_t) :: result
    type(json_value_t) :: report, tests, entry, field

    selected = 'solovev_axis_regularity'
    neighbor = selected // '_extra'
    call resolve_driver(driver)
    call current_directory(project)
    call make_scratch('fo-cmake-named-cli', scratch)
    selected_receipt = join_path(scratch, 'build/' // selected // '.ran')
    neighbor_receipt = join_path(scratch, 'build/' // neighbor // '.ran')
    call exercise_harness()

    call write_lines(join_path(scratch, 'CMakeLists.txt'), [character(len=192) :: &
        'cmake_minimum_required(VERSION 3.20)', &
        'project(fo_cmake_named_cli NONE)', &
        'enable_testing()', &
        'add_test(NAME solovev_axis_regularity COMMAND "${CMAKE_COMMAND}" -E touch ' // &
        '"${CMAKE_BINARY_DIR}/solovev_axis_regularity.ran")', &
        'add_test(NAME solovev_axis_regularity_extra COMMAND "${CMAKE_COMMAND}" -E ' // &
        'touch "${CMAKE_BINARY_DIR}/solovev_axis_regularity_extra.ran")'])
    call list_add(environment, 'FO_BACKEND=cmake')
    call list_add(environment, 'FO_JOBS=2')
    call list_add(arguments, 'test')
    call list_add(arguments, selected)
    call list_add(arguments, '--json')
    call run_fo(driver, arguments, scratch, join_path(scratch, 'fo-cache'), result, environment)
    call assert_process_ok(result, 'named CTest request')
    call parse_json_report(result, report, 'named CTest report')
    tests = json_member(report, 'tests')
    call assert_equal_integer(json_size(tests), 1, &
        'report retains only the exact requested CTest ID')
    entry = json_element(tests, 1)
    field = json_member(entry, 'name')
    call assert_equal_string(json_string_value(field), selected, 'reported CTest ID')
    field = json_member(entry, 'status')
    call assert_equal_string(json_string_value(field), 'pass', 'requested CTest passes')
    call assert_file_exists(selected_receipt, 'requested CTest case executes')
    call assert_file_absent(neighbor_receipt, 'similarly named CTest case stays unrun')

    call remove_path(selected_receipt)
    arguments = string_list_t()
    call list_add(arguments, 'test')
    call list_add(arguments, 'not_a_registered_ctest_id')
    call list_add(arguments, '--json')
    call run_fo(driver, arguments, scratch, join_path(scratch, 'fo-cache'), result, environment)
    call assert_equal_integer(result%term_signal, 0, 'unknown CTest ID exits normally')
    call assert_true(result%exit_code /= 0, 'unknown CTest ID fails instead of reporting zero tests')
    call assert_file_absent(selected_receipt, 'no-match does not run a registered case')
    call assert_file_absent(neighbor_receipt, 'no-match does not run a similar case')

    write(*, '(a)') 'cmake-named-cli: exact CTest ID ran alone; no-match failed'
    call remove_tree(scratch)
end program test_cmake_named_cli
