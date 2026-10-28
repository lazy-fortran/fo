program test_cmake_named_cli
    use, intrinsic :: iso_fortran_env, only: output_unit, error_unit
    use fo_test_harness, only: process_result_t, string_list_t, list_add
    use fo_test_harness, only: make_scratch, join_path, current_directory
    use fo_test_harness, only: write_lines, remove_path, remove_tree
    use fo_test_harness, only: assert_equal_integer, assert_equal_string
    use fo_test_harness, only: assert_file_exists, assert_file_absent
    use fo_test_harness, only: assert_process_ok, assert_true, assert_contains
    use fo_test_cli, only: resolve_driver, run_fo, parse_json_report, exercise_harness
    use fo_test_json, only: json_value_t, json_member, json_element, json_size
    use fo_test_json, only: json_string_value
    use fo_test_harness, only: finish_assertions
    use fo_test_harness, only: run_process
    use fo_test_harness, only: harness_probe_entry, environment_value, make_symlink, &
        resolve_test_executable
    implicit none

    character(:), allocatable :: driver, project, scratch, selected, neighbor
    character(:), allocatable :: selected_receipt, neighbor_receipt
    type(string_list_t) :: arguments, environment
    type(process_result_t) :: result
    type(json_value_t) :: report, tests, entry, field

    call ctest_xml_wrapper_entry()
    call harness_probe_entry()
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
    call list_add(arguments, '--json=full')
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
    call list_add(arguments, '--json=full')
    call run_fo(driver, arguments, scratch, join_path(scratch, 'fo-cache'), result, environment)
    call assert_equal_integer(result%term_signal, 0, 'unknown CTest ID exits normally')
    call assert_true(result%exit_code /= 0, 'unknown CTest ID fails instead of reporting zero tests')
    call assert_file_absent(selected_receipt, 'no-match does not run a registered case')
    call assert_file_absent(neighbor_receipt, 'no-match does not run a similar case')

    call remove_tree(scratch)
    call directory_local_names()
    call directory_registry_rejection()
    call profile_isolation()
    call build_only_planned_targets()
    call preset_filter_execution()
    call finish_assertions()
    write(*, '(a)') 'cmake-named-cli: exact CTest ID ran alone; no-match failed'
contains
    subroutine directory_registry_rejection()
        character(:), allocatable :: root
        character(len=:), allocatable :: lines(:)
        type(string_list_t) :: args, env
        type(process_result_t) :: observed

        call make_scratch('fo-ctest-registry-proof', root)
        lines = [character(len=192) :: &
            'cmake_minimum_required(VERSION 3.20)', &
            'project(registry_proof NONE)', 'enable_testing()', &
            'configure_file(runtime.in runtime.cmake @ONLY)', &
            'configure_file(actual.in actual.cmake @ONLY)', &
            'configure_file(false.in false/CTestTestfile.cmake @ONLY)', &
            'set_property(DIRECTORY PROPERTY TEST_INCLUDE_FILES '// &
            '"${CMAKE_BINARY_DIR}/runtime.cmake")', 'add_subdirectory(b)']
        call write_lines(join_path(root, 'CMakeLists.txt'), lines)
        lines = [character(len=192) :: &
            'if(FALSE)', '  subdirs("@CMAKE_BINARY_DIR@/false")', 'endif()', &
            'set(actual "@CMAKE_BINARY_DIR@/actual.cmake")', 'include("${actual}")']
        call write_lines(join_path(root, 'runtime.in'), lines)
        lines = [character(len=192) :: &
            'add_test(duplicate "@CMAKE_COMMAND@" -E touch "@CMAKE_BINARY_DIR@/real.ran")']
        call write_lines(join_path(root, 'actual.in'), lines)
        lines = [character(len=192) :: &
            'add_test(duplicate "@CMAKE_COMMAND@" -E touch "@CMAKE_BINARY_DIR@/false.ran")']
        call write_lines(join_path(root, 'false.in'), lines)
        lines = [character(len=192) :: &
            'add_test(NAME duplicate COMMAND "${CMAKE_COMMAND}" -E touch '// &
            '"${CMAKE_BINARY_DIR}/b.ran")']
        call write_lines(join_path(root, 'b/CMakeLists.txt'), lines)
        call list_add(args, 'cmake')
        call list_add(args, '-S')
        call list_add(args, root)
        call list_add(args, '-B')
        call list_add(args, join_path(root, 'build'))
        call run_process(args, root, observed)
        call assert_process_ok(observed, 'configure adversarial registry oracle')
        args = string_list_t()
        call list_add(args, 'ctest')
        call list_add(args, '--test-dir')
        call list_add(args, join_path(root, 'build'))
        call list_add(args, '-I')
        call list_add(args, '1,1')
        call run_process(args, root, observed)
        call assert_process_ok(observed, 'native CTest evaluates dynamic registry')
        call assert_file_exists(join_path(root, 'build/real.ran'), 'actual root test ran')
        call assert_file_absent(join_path(root, 'build/false.ran'), 'false directory unrun')
        call remove_path(join_path(root, 'build/real.ran'))
        args = string_list_t()
        call list_add(args, 'test')
        call list_add(args, 'false::duplicate')
        call list_add(args, '--json=full')
        call list_add(env, 'FO_BACKEND=cmake')
        call run_fo(driver, args, root, join_path(root, 'cache'), observed, env)
        call assert_true(observed%exit_code /= 0, 'dynamic registry cannot invent a test ID')
        call assert_contains(observed%stderr, 'unsupported CTest directory registry script', &
            'unsupported dynamic registry is explicit')
        call assert_file_absent(join_path(root, 'build/real.ran'), 'unproven test stays unrun')
        call assert_file_absent(join_path(root, 'build/false.ran'), 'false directory stays unrun')
        call remove_tree(root)
    end subroutine directory_registry_rejection

    subroutine ctest_xml_wrapper_entry()
        type(string_list_t) :: args
        type(process_result_t) :: observed
        character(:), allocatable :: mode, argument, xml, cwd
        character(len=:), allocatable :: lines(:)
        integer :: i, length
        logical :: next_xml

        mode = environment_value('FO_CTEST_XML_MODE')
        if (len(mode) == 0) return
        call list_add(args, environment_value('FO_REAL_CTEST'))
        xml = ''
        next_xml = .false.
        do i = 1, command_argument_count()
            call get_command_argument(i, length=length)
            allocate (character(len=length) :: argument)
            call get_command_argument(i, argument)
            call list_add(args, argument)
            if (next_xml) xml = argument
            next_xml = argument == '--output-junit'
            deallocate (argument)
        end do
        call current_directory(cwd)
        call run_process(args, cwd, observed)
        write (output_unit, '(a)', advance='no') observed%stdout
        write (error_unit, '(a)', advance='no') observed%stderr
        if (len(xml) > 0) then
            if (mode == 'missing') call remove_path(xml)
            if (mode == 'malformed') then
                lines = [character(len=16) :: '<testsuite><bad>']
                call write_lines(xml, lines)
            end if
        end if
        if (observed%exit_code /= 0) stop 1
        stop
    end subroutine ctest_xml_wrapper_entry

    subroutine directory_local_names()
        character(:), allocatable :: root, receipt, executable, real_ctest
        character(len=:), allocatable :: lines(:)
        type(string_list_t) :: args, env, command
        type(process_result_t) :: observed
        type(json_value_t) :: doc, results, item
        character(len=12), parameter :: selectors(*) = &
            [character(len=12) :: 'a::duplicate', 'b::duplicate', 'unique']
        character(len=6), parameter :: markers(*) = &
            [character(len=6) :: 'a.ran', 'b.ran', 'u.ran']
        integer :: i, j, mode, ending
        logical :: found

        call make_scratch('fo-ctest-local-names', root)
        lines = [character(len=192) :: &
            'cmake_minimum_required(VERSION 3.20)', &
            'project(local_names NONE)', 'enable_testing()', &
            'file(MAKE_DIRECTORY "${CMAKE_BINARY_DIR}/shared")', &
            'configure_file(registration.cmake.in outside/registration.cmake @ONLY)', &
            'add_subdirectory(a)', 'add_subdirectory(b)', &
            'add_test(NAME unique COMMAND "${CMAKE_COMMAND}" -E touch '// &
            '"${CMAKE_BINARY_DIR}/u.ran")']
        call write_lines(join_path(root, 'CMakeLists.txt'), lines)
        lines = [character(len=192) :: &
            'set_property(DIRECTORY PROPERTY TEST_INCLUDE_FILES '// &
            '"${CMAKE_BINARY_DIR}/outside/registration.cmake")']
        call write_lines(join_path(root, 'a/CMakeLists.txt'), lines)
        lines = [character(len=192) :: &
            'add_test(duplicate "@CMAKE_COMMAND@" -E touch '// &
            '"@CMAKE_BINARY_DIR@/a.ran")', &
            'set_tests_properties(duplicate PROPERTIES WORKING_DIRECTORY '// &
            '"@CMAKE_BINARY_DIR@/shared")']
        call write_lines(join_path(root, 'registration.cmake.in'), lines)
        lines = [character(len=192) :: &
            'add_test(NAME duplicate COMMAND "${CMAKE_COMMAND}" -E touch '// &
            '"${CMAKE_BINARY_DIR}/b.ran")', &
            'set_tests_properties(duplicate PROPERTIES WORKING_DIRECTORY '// &
            '"${CMAKE_BINARY_DIR}/shared")']
        call write_lines(join_path(root, 'b/CMakeLists.txt'), lines)
        call list_add(command, 'cmake')
        call list_add(command, '-S')
        call list_add(command, root)
        call list_add(command, '-B')
        call list_add(command, join_path(root, 'build'))
        call run_process(command, root, observed)
        call assert_process_ok(observed, 'configure directory-local CTest oracle')
        do mode = 0, 2
            env = string_list_t()
            call list_add(env, 'FO_BACKEND=cmake')
            if (mode == 2) call list_add(env, 'FO_CTEST_DELEGATE=1')
            do i = 1, size(selectors)
                if (mode == 0) then
                    args = string_list_t()
                    call list_add(args, 'ctest')
                    call list_add(args, '--test-dir')
                    call list_add(args, join_path(root, 'build'))
                    call list_add(args, '-I')
                    select case (i)
                    case (1)
                        call list_add(args, '2,2')
                    case (2)
                        call list_add(args, '3,3')
                    case (3)
                        call list_add(args, '1,1')
                    end select
                    call run_process(args, root, observed)
                else
                    args = string_list_t()
                    call list_add(args, 'test')
                    call list_add(args, trim(selectors(i)))
                    call list_add(args, '--json=full')
                    call run_fo(driver, args, root, join_path(root, 'cache'), observed, env)
                    call parse_json_report(observed, doc, 'directory-local CTest report')
                    results = json_member(doc, 'tests')
                    call assert_equal_integer(json_size(results), 1, &
                        'only the requested directory-local test is reported')
                    item = json_element(results, 1)
                    call assert_equal_string(json_string_value(json_member(item, 'name')), &
                        trim(selectors(i)), 'directory-local public test ID')
                end if
                call assert_process_ok(observed, 'selected directory-local test')
                do j = 1, size(markers)
                    receipt = join_path(root, 'build/'//trim(markers(j)))
                    if (j == i) then
                        call assert_file_exists(receipt, 'requested local test executed')
                    else
                        call assert_file_absent(receipt, 'unselected local test stayed unrun')
                    end if
                    call remove_path(receipt)
                end do
            end do
        end do
        args = string_list_t()
        call list_add(args, 'sh')
        call list_add(args, '-c')
        call list_add(args, 'command -v ctest')
        call run_process(args, root, observed)
        call assert_process_ok(observed, 'locate native CTest for XML wrapper')
        ending = index(observed%stdout, achar(10))
        real_ctest = observed%stdout(:ending - 1)
        call resolve_test_executable(executable, found)
        call assert_true(found, 'resolve native XML wrapper executable')
        lines = [character(len=1) :: '']
        call write_lines(join_path(root, 'wrapper/ready'), lines)
        call make_symlink(executable, join_path(root, 'wrapper/ctest'))
        do mode = 1, 2
            env = string_list_t()
            call list_add(env, 'FO_BACKEND=cmake')
            call list_add(env, 'FO_CTEST_DELEGATE=1')
            call list_add(env, 'FO_REAL_CTEST='//real_ctest)
            call list_add(env, 'PATH='//join_path(root, 'wrapper:')//environment_value('PATH'))
            if (mode == 1) call list_add(env, 'FO_CTEST_XML_MODE=missing')
            if (mode == 2) call list_add(env, 'FO_CTEST_XML_MODE=malformed')
            args = string_list_t()
            call list_add(args, 'test')
            call list_add(args, 'a::duplicate')
            call list_add(args, '--json=full')
            call run_fo(driver, args, root, join_path(root, 'cache'), observed, env)
            call assert_true(observed%exit_code /= 0, 'bad delegated JUnit fails closed')
            call parse_json_report(observed, doc, 'bad delegated JUnit report')
            results = json_member(doc, 'tests')
            call assert_equal_integer(json_size(results), 1, 'one qualified failure reported')
            item = json_element(results, 1)
            call assert_equal_string(json_string_value(json_member(item, 'name')), &
                'a::duplicate', 'failed delegated result keeps qualified identity')
            call assert_equal_string(json_string_value(json_member(item, 'status')), &
                'fail', 'missing or malformed JUnit cannot report pass')
            call assert_file_exists(join_path(root, 'build/a.ran'), &
                'the native test ran before its JUnit was removed or corrupted')
            call assert_file_absent(join_path(root, 'build/b.ran'), 'other test stays unrun')
            call remove_path(join_path(root, 'build/a.ran'))
        end do
        do i = 1, 2
            lines = [character(len=192) :: &
                'add_test(NAME producer COMMAND "${CMAKE_COMMAND}" -E touch '// &
                '"${CMAKE_CURRENT_BINARY_DIR}/producer.ran")', &
                'add_test(NAME consumer COMMAND "${CMAKE_COMMAND}" -P '// &
                '"${CMAKE_SOURCE_DIR}/consume.cmake")', &
                'set_tests_properties(consumer PROPERTIES DEPENDS producer)']
            if (i == 1) then
                call write_lines(join_path(root, 'a/CMakeLists.txt'), lines)
            else
                call write_lines(join_path(root, 'b/CMakeLists.txt'), lines)
            end if
        end do
        lines = [character(len=192) :: &
            'if(NOT EXISTS producer.ran)', '  message(FATAL_ERROR "producer missing")', &
            'endif()', 'file(WRITE consumer.ran "consumed")']
        call write_lines(join_path(root, 'consume.cmake'), lines)
        call run_process(command, root, observed)
        call assert_process_ok(observed, 'configure directory-local dependency oracle')
        do mode = 0, 2
            do i = 1, 2
                args = string_list_t()
                if (mode == 0) then
                    call list_add(args, 'ctest')
                    call list_add(args, '--test-dir')
                    call list_add(args, join_path(root, 'build'))
                    call list_add(args, '-I')
                    if (i == 1) call list_add(args, '0,0,0,2,3')
                    if (i == 2) call list_add(args, '0,0,0,4,5')
                    call run_process(args, root, observed)
                else
                    env = string_list_t()
                    call list_add(env, 'FO_BACKEND=cmake')
                    if (mode == 2) call list_add(env, 'FO_CTEST_DELEGATE=1')
                    call list_add(args, 'test')
                    if (i == 1) then
                        call list_add(args, 'a::consumer')
                        call list_add(args, 'a::producer')
                    else
                        call list_add(args, 'b::consumer')
                        call list_add(args, 'b::producer')
                    end if
                    call list_add(args, '--json=full')
                    call run_fo(driver, args, root, join_path(root, 'cache'), observed, env)
                    if (mode == 2) then
                        call assert_true(observed%exit_code /= 0, &
                            'ambiguous delegated dependency fails closed')
                        call assert_contains(observed%stderr, &
                            'directory-local CTest dependencies', &
                            'unsupported delegated dependency is explicit')
                        do j = 1, 2
                            if (j == 1) receipt = join_path(root, 'build/a/')
                            if (j == 2) receipt = join_path(root, 'build/b/')
                            call assert_file_absent(receipt//'producer.ran', &
                                'rejected producer stays unrun')
                            call assert_file_absent(receipt//'consumer.ran', &
                                'rejected consumer stays unrun')
                        end do
                        cycle
                    end if
                    call parse_json_report(observed, doc, 'local dependency report')
                    call assert_equal_integer(json_size(json_member(doc, 'tests')), 2, &
                        'local dependency pair is reported once')
                end if
                call assert_process_ok(observed, 'local producer precedes local consumer')
                do j = 1, 2
                    if (j == 1) receipt = join_path(root, 'build/a/')
                    if (j == 2) receipt = join_path(root, 'build/b/')
                    if (j == i) then
                        call assert_file_exists(receipt//'producer.ran', 'local producer ran')
                        call assert_file_exists(receipt//'consumer.ran', 'local consumer ran')
                    else
                        call assert_file_absent(receipt//'producer.ran', 'other producer unrun')
                        call assert_file_absent(receipt//'consumer.ran', 'other consumer unrun')
                    end if
                    call remove_path(receipt//'producer.ran')
                    call remove_path(receipt//'consumer.ran')
                end do
            end do
        end do
        lines = [character(len=192) :: &
            'cmake_minimum_required(VERSION 3.20)', &
            'project(space_names NONE)', 'enable_testing()', &
            'add_subdirectory(a "space a")', 'add_subdirectory(b)']
        call write_lines(join_path(root, 'CMakeLists.txt'), lines)
        env = string_list_t()
        call list_add(env, 'FO_BACKEND=cmake')
        args = string_list_t()
        call list_add(args, 'test')
        call list_add(args, 'b::producer')
        call list_add(args, '--json=full')
        call run_fo(driver, args, root, join_path(root, 'cache'), observed, env)
        call assert_true(observed%exit_code /= 0, 'unrepresentable qualified ID is rejected')
        call assert_contains(observed%stderr, 'cannot contain whitespace', &
            'registration-directory whitespace has an explicit diagnostic')
        call remove_tree(root)
    end subroutine directory_local_names

    function ran(result, name) result(found)
        type(process_result_t), intent(in) :: result
        character(*), intent(in) :: name
        logical :: found

        found = index(result%stdout, '"name":"' // name // '"') > 0
    end function ran

    function line_count(path) result(n)
        character(*), intent(in) :: path
        integer :: n, unit, ios
        character(len=64) :: line

        n = 0
        open (newunit=unit, file=path, status='old', action='read', iostat=ios)
        if (ios /= 0) return
        do
            read (unit, '(a)', iostat=ios) line
            if (ios /= 0) exit
            n = n + 1
        end do
        close (unit)
    end function line_count

    subroutine build_only_planned_targets()
        !! A broken unrelated target never blocks a selection that does not
        !! need it, also on an unbuilt tree; regression-labelled cases run by
        !! default without a preset, slow ones need --all, and a fixture runs
        !! once per ctest batch.
        character(:), allocatable :: root, counter
        type(string_list_t) :: args, env
        type(process_result_t) :: observed

        call make_scratch('fo-cmake-planned', root)
        counter = join_path(root, 'build/setup.count')
        call write_lines(join_path(root, 'CMakeLists.txt'), [character(len=160) :: &
            'cmake_minimum_required(VERSION 3.20)', &
            'project(planned Fortran)', &
            'enable_testing()', &
            'add_executable(good good.f90)', &
            'add_executable(broken broken.f90)', &
            'add_test(NAME good_case COMMAND good)', &
            'add_test(NAME reg_case COMMAND good)', &
            'set_tests_properties(reg_case PROPERTIES LABELS regression)', &
            'add_test(NAME slow_case COMMAND good)', &
            'set_tests_properties(slow_case PROPERTIES LABELS slow)', &
            'add_test(NAME fx_setup COMMAND good "${CMAKE_BINARY_DIR}/setup.count")', &
            'set_tests_properties(fx_setup PROPERTIES FIXTURES_SETUP fx)', &
            'add_test(NAME need_a COMMAND good)', &
            'add_test(NAME need_b COMMAND good)', &
            'set_tests_properties(need_a need_b PROPERTIES FIXTURES_REQUIRED fx)'])
        call write_lines(join_path(root, 'good.f90'), [character(len=96) :: &
            'program good', &
            'character(len=512) :: path', &
            'integer :: unit', &
            'if (command_argument_count() > 0) then', &
            'call get_command_argument(1, path)', &
            'open (newunit=unit, file=trim(path), position="append")', &
            'write (unit, "(a)") "setup"', &
            'close (unit)', &
            'end if', &
            'end program'])
        call write_lines(join_path(root, 'broken.f90'), [character(len=48) :: &
            'program broken', 'this is not valid fortran (', 'end program'])
        call list_add(env, 'FO_BACKEND=cmake')
        call list_add(env, 'FO_JOBS=2')

        call list_add(args, 'test')
        call list_add(args, 'good_case')
        call list_add(args, '--json=full')
        call run_fo(driver, args, root, join_path(root, 'fo-cache'), observed, env)
        call assert_process_ok(observed, 'named case builds without the broken target')
        call assert_true(ran(observed, 'good_case'), 'named case ran')
        call assert_file_absent(join_path(root, 'build/broken'), &
            'unrelated broken target was not built')

        args = string_list_t()
        call list_add(args, 'test')
        call list_add(args, '--json=full')
        call run_fo(driver, args, root, join_path(root, 'fo-cache'), observed, env)
        call assert_process_ok(observed, 'default run plans only the targets it needs')
        call assert_true(ran(observed, 'reg_case'), &
            'regression-labelled case runs by default without a preset')
        call assert_true(.not. ran(observed, 'slow_case'), 'slow case needs --all')
        call assert_true(ran(observed, 'good_case'), 'default run keeps plain cases')

        args = string_list_t()
        call list_add(args, 'test')
        call list_add(args, '--all')
        call list_add(args, '--json=full')
        call run_fo(driver, args, root, join_path(root, 'fo-cache'), observed, env)
        call assert_process_ok(observed, '--all runs the slow case')
        call assert_true(ran(observed, 'slow_case'), '--all includes the slow case')

        call remove_path(counter)
        args = string_list_t()
        call list_add(args, 'test')
        call list_add(args, 'need_a')
        call list_add(args, 'need_b')
        call list_add(args, '--json=full')
        call run_fo(driver, args, root, join_path(root, 'fo-cache'), observed, env)
        call assert_process_ok(observed, 'fixture cases run')
        call assert_equal_integer(line_count(counter), 1, &
            'fixture setup runs once for the whole batch')

        ! --no-build runs what is there and never compiles.
        call remove_path(join_path(root, 'build/good'))
        args = string_list_t()
        call list_add(args, 'test')
        call list_add(args, '--no-build')
        call list_add(args, 'good_case')
        call list_add(args, '--json=full')
        call run_fo(driver, args, root, join_path(root, 'fo-cache'), observed, env)
        call assert_true(observed%exit_code /= 0, '--no-build does not rebuild')
        call assert_file_absent(join_path(root, 'build/good'), &
            '--no-build leaves the executable unbuilt')
        call remove_tree(root)
    end subroutine build_only_planned_targets

    subroutine preset_filter_execution()
        !! A case the test preset filters out is not run by default and is
        !! run by --all.
        character(:), allocatable :: root
        type(string_list_t) :: args, env
        type(process_result_t) :: observed

        call make_scratch('fo-cmake-preset-run', root)
        call write_lines(join_path(root, 'CMakeLists.txt'), [character(len=160) :: &
            'cmake_minimum_required(VERSION 3.20)', &
            'project(preset_run NONE)', &
            'enable_testing()', &
            'add_test(NAME kept COMMAND "${CMAKE_COMMAND}" -E echo kept)', &
            'add_test(NAME gated COMMAND "${CMAKE_COMMAND}" -E echo gated)', &
            'set_tests_properties(gated PROPERTIES LABELS gate)'])
        call write_lines(join_path(root, 'CMakePresets.json'), [character(len=200) :: &
            '{"version":3,"configurePresets":[{"name":"c","generator":"Ninja",', &
            '"binaryDir":"${sourceDir}/bld"}],"testPresets":[{"name":"t",', &
            '"configurePreset":"c","filter":{"exclude":{"label":"gate"}}}]}'])
        call list_add(env, 'FO_BACKEND=cmake')
        call list_add(env, 'FO_JOBS=2')
        call list_add(env, 'FO_CMAKE_CONFIGURE_PRESET=c')
        call list_add(env, 'FO_CMAKE_TEST_PRESET=t')
        call list_add(env, 'FO_CMAKE_BUILD_DIR=bld')
        call list_add(args, 'test')
        call list_add(args, '--json=full')
        call run_fo(driver, args, root, join_path(root, 'fo-cache'), observed, env)
        call assert_process_ok(observed, 'preset default run')
        call assert_true(ran(observed, 'kept'), 'preset keeps the unfiltered case')
        call assert_true(.not. ran(observed, 'gated'), &
            'preset-excluded case is not run by default')

        args = string_list_t()
        call list_add(args, 'test')
        call list_add(args, '--all')
        call list_add(args, '--json=full')
        call run_fo(driver, args, root, join_path(root, 'fo-cache'), observed, env)
        call assert_process_ok(observed, 'preset --all run')
        call assert_true(ran(observed, 'gated'), '--all runs the preset-excluded case')
        call remove_tree(root)
    end subroutine preset_filter_execution

    subroutine profile_isolation()
        character(:), allocatable :: root
        type(string_list_t) :: args, env
        type(process_result_t) :: observed

        call make_scratch('fo-cmake-profile', root)
        call write_lines(join_path(root, 'CMakeLists.txt'), [character(len=160) :: &
            'cmake_minimum_required(VERSION 3.20)', &
            'project(profile_isolation Fortran)', &
            'enable_testing()', &
            'add_executable(numeric numeric.f90)', &
            'target_compile_definitions(numeric PRIVATE '// &
            '"ACTIVE_CONFIGURATION=\"${CMAKE_BUILD_TYPE}\"")', &
            'add_test(NAME numeric COMMAND numeric "${CMAKE_BUILD_TYPE}")'])
        call write_lines(join_path(root, 'CMakePresets.json'), [character(len=192) :: &
            '{"version":3,"configurePresets":[{"name":"cpu","generator":"Ninja",', &
            '"binaryDir":"${sourceDir}/build","cacheVariables":{', &
            '"CMAKE_BUILD_TYPE":"Release",', &
            '"CMAKE_Fortran_COMPILER":"gfortran",', &
            '"CMAKE_Fortran_FLAGS":"-cpp -DKEEP_PROJECT",', &
            '"CMAKE_Fortran_FLAGS_RELEASE":"-O2"}}],', &
            '"buildPresets":[{"name":"cpu","configurePreset":"cpu"}]}'])
        call write_lines(join_path(root, 'numeric.f90'), [character(len=160) :: &
            'program numeric', &
            'use iso_fortran_env, only: compiler_options, real64', &
            'implicit none', &
            'character(32)::configuration', &
            'character(:),allocatable::options', &
            'real(real64)::a(2,2),x(2),y(2)', &
            'real(real64),allocatable::bounds_probe(:)', &
            'integer::requested_index', &
            'call get_command_argument(1,configuration)', &
            'if(trim(configuration)/=ACTIVE_CONFIGURATION) '// &
            'error stop "executable configured for another profile"', &
            'options=compiler_options()', &
            '#ifndef KEEP_PROJECT', &
            'error stop "project global flags were overwritten"', &
            '#endif', &
            'select case(trim(configuration))', &
            'case("Debug","Checked")', &
            'if(index(options,"-fcheck=all")==0) error stop "missing Debug checks"', &
            'case("Release")', &
            'if(index(options,"-fcheck")/=0) '// &
            'error stop "Release inherited Debug checks"', &
            'if(index(options,"-fbacktrace")/=0) '// &
            'error stop "Release inherited backtrace"', &
            'if(index(options,"-O0")/=0) '// &
            'error stop "Release inherited Debug optimization"', &
            'if(index(options,"-O2")==0) error stop "Release preset flags lost"', &
            'case default', &
            'error stop "wrong profile configuration"', &
            'end select', &
            'a=reshape([2d0,-1d0,1d0,2d0],[2,2]);x=[.4d0,2.2d0]', &
            'y=matmul(a,x)', &
            'if(maxval(abs(y-[3d0,4d0]))>1d-14) error stop "numeric oracle"', &
            'if(command_argument_count()>1)then', &
            'call get_command_argument(2,configuration)', &
            'read(configuration,*) requested_index', &
            'allocate(bounds_probe(1));bounds_probe=1', &
            'print *,bounds_probe(requested_index)', &
            'end if', &
            'end program'])
        call list_add(env, 'FO_BACKEND=cmake')
        ! Fo's native compiler preference must not override the CMake preset.
        call list_add(env, 'FO_FC=flang')
        call list_add(env, 'FO_CMAKE_CONFIGURE_PRESET=cpu')
        call list_add(env, 'FO_CMAKE_BUILD_PRESET=cpu')
        call list_add(env, 'FO_CMAKE_BUILD_DIR=build')
        call list_add(env, 'FO_CMAKE_CONFIG=')
        call list_add(env, 'FO_JOBS=2')
        call list_add(args, 'test')
        call list_add(args, '--profile')
        call list_add(args, 'debug')
        call list_add(args, 'numeric')
        call run_fo(driver, args, root, join_path(root, 'fo-cache'), observed, env)
        call assert_process_ok(observed, &
            'public Debug profile uses checked Debug executable')

        args = string_list_t()
        call list_add(args, join_path(root, 'build/numeric'))
        call list_add(args, 'Debug')
        call list_add(args, '2')
        call run_process(args, root, observed)
        call assert_true(observed%exit_code /= 0, &
            'Debug executable rejects a dynamic out-of-bounds index')
        call assert_true(index(observed%stderr, 'above upper bound') > 0, &
            'Debug failure is the independent runtime bounds check')

        ! Native preset reconfiguration shares the exact build directory.
        args = string_list_t()
        call list_add(args, 'cmake')
        call list_add(args, '--preset')
        call list_add(args, 'cpu')
        call run_process(args, root, observed)
        call assert_process_ok(observed, 'native Release preset after Fo Debug')
        args = string_list_t()
        call list_add(args, 'cmake')
        call list_add(args, '--build')
        call list_add(args, 'build')
        call run_process(args, root, observed)
        call assert_process_ok(observed, 'native Release rebuild')
        args = string_list_t()
        call list_add(args, 'ctest')
        call list_add(args, '--test-dir')
        call list_add(args, 'build')
        call list_add(args, '--output-on-failure')
        call run_process(args, root, observed)
        call assert_process_ok(observed, &
            'Release numeric executable excludes Debug flags')

        args = string_list_t()
        call list_add(args, 'build')
        call list_add(args, '--flag')
        call list_add(args, '-fcheck=all')
        call run_fo(driver, args, root, join_path(root, 'fo-cache'), observed, env)
        call assert_true(observed%exit_code /= 0, &
            'ambiguous CMake flag scope is refused')
        call assert_true(index(observed%stderr, '--profile or FO_CMAKE_CONFIG') > 0, &
            'ambiguous flag diagnostic names configuration choices')

        ! An explicit custom configuration is authoritative over profile inference.
        env = string_list_t()
        call list_add(env, 'FO_BACKEND=cmake')
        call list_add(env, 'FO_CMAKE_CONFIGURE_PRESET=cpu')
        call list_add(env, 'FO_CMAKE_BUILD_PRESET=cpu')
        call list_add(env, 'FO_CMAKE_BUILD_DIR=build')
        call list_add(env, 'FO_CMAKE_CONFIG=Checked')
        call list_add(env, 'FO_JOBS=2')
        args = string_list_t()
        call list_add(args, 'test')
        call list_add(args, '--profile')
        call list_add(args, 'debug')
        call list_add(args, 'numeric')
        call run_fo(driver, args, root, join_path(root, 'fo-cache'), observed, env)
        call assert_process_ok(observed, &
            'explicit custom configuration with Debug checks')
        args = string_list_t()
        call list_add(args, join_path(root, 'build/numeric'))
        call list_add(args, 'Checked')
        call run_process(args, root, observed)
        call assert_process_ok(observed, &
            'numeric executable retains explicit custom configuration')
        call remove_tree(root)
    end subroutine profile_isolation
end program test_cmake_named_cli
