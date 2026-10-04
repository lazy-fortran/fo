program test_exec_selected_test_cli
    use fo_test_harness, only: make_scratch, join_path, write_text, read_text
    use fo_test_harness, only: remove_tree
    use fo_test_harness, only: assert_true, assert_equal_string, assert_process_ok
    use fo_test_harness, only: process_result_t, string_list_t, list_add
    use fo_test_cli, only: resolve_driver, run_fo, run_external
    use fo_fs, only: fs_find_executable
    use fo_test_harness, only: finish_assertions
    implicit none

    character(:), allocatable :: driver, scratch, project, compiler_log
    character(len=512) :: compiler, c_compiler, wrapper
    logical :: found, c_found
    type(process_result_t) :: child
    type(string_list_t) :: command, environment

    call resolve_driver(driver)
    call make_scratch('fo-exec-selected-test', scratch)
    project = join_path(scratch, 'project')
    compiler_log = join_path(scratch, 'compiler-argv')
    wrapper = join_path(scratch, 'gfortran-recorder')
    call write_text(join_path(project, 'fpm.toml'), 'name = "exec_selected_test"'// &
        new_line('a'))
    call write_text(join_path(project, 'app/unselected_app.f90'), &
        'program unselected_app'//new_line('a')//'end program unselected_app'// &
        new_line('a'))
    call write_text(join_path(project, 'test/unselected_test.f90'), &
        'program unselected_test'//new_line('a')//'end program unselected_test'// &
        new_line('a'))
    call write_text(join_path(project, 'test/selected_test.f90'), selected_source())

    call fs_find_executable('gfortran', compiler, found)
    call assert_true(found, 'selected-test oracle finds gfortran')
    call fs_find_executable('cc', c_compiler, c_found)
    call assert_true(c_found, 'selected-test oracle finds a C compiler')
    if (.not. found .or. .not. c_found) then
        call finish_assertions()
        stop 1
    end if

    call write_text(join_path(scratch, 'compiler-wrapper.c'), wrapper_source())
    command = string_list_t()
    call list_add(command, join_path(scratch, 'compiler-wrapper.c'))
    call list_add(command, '-o')
    call list_add(command, wrapper)
    call run_external(trim(c_compiler), command, scratch, child)
    call assert_process_ok(child, 'native compiler argv recorder builds')

    command = words([character(len=32) :: &
        'exec', 'selected_test', 'argument with spaces', 'second'])
    call list_add(environment, 'FO_FC='//wrapper)
    call list_add(environment, 'FO_TEST_REAL_COMPILER='//trim(compiler))
    call list_add(environment, 'FO_TEST_COMPILER_LOG='//compiler_log)
    call list_add(environment, 'FO_EXEC_TEST_MARKER='//join_path(scratch, 'runs'))
    call run_fo(driver, command, project, join_path(scratch, 'fo-cache'), child, &
        environment)
    call assert_process_ok(child, 'fo exec runs the selected native test')
    call assert_equal_string(child%stdout, &
        '2'//new_line('a')//'argument with spaces'//new_line('a')//'second'// &
        new_line('a'), 'selected test receives original argv exactly once')
    call assert_equal_string(read_text(join_path(scratch, 'runs')), 'run'// &
        new_line('a'), 'selected test process ran once')

    call assert_true(index(read_text(compiler_log), &
        '/test/selected_test.f90') > 0, 'selected test source was compiled')
    call assert_true(index(read_text(compiler_log), &
        '/app/unselected_app.f90') == 0, 'unselected app source was not compiled')
    call assert_true(index(read_text(compiler_log), &
        '/test/unselected_test.f90') == 0, 'unselected test source was not compiled')

    call write_text(compiler_log, '')
    command = words([character(len=32) :: &
        'exec', 'selected_test', 'argument with spaces', 'second'])
    call run_fo(driver, command, project, join_path(scratch, 'fo-cache'), child, &
        environment)
    call assert_process_ok(child, 'warm fo exec runs the selected native test')
    call assert_equal_string(child%stdout, &
        '2'//new_line('a')//'argument with spaces'//new_line('a')//'second'// &
        new_line('a'), 'warm selected test receives original argv')
    call assert_equal_string(read_text(join_path(scratch, 'runs')), &
        'run'//new_line('a')//'run'//new_line('a'), &
        'warm selected test process runs once')
    call assert_equal_string(read_text(compiler_log), '', &
        'warm selected test needs no compiler or linker invocation')

    call remove_tree(scratch)
    call finish_assertions()

contains

    function words(items) result(list)
        character(len=*), intent(in) :: items(:)
        type(string_list_t) :: list
        integer :: i

        do i = 1, size(items)
            call list_add(list, trim(items(i)))
        end do
    end function words

    function selected_source() result(text)
        character(:), allocatable :: text
        text = 'program selected_test'//new_line('a')// &
            'implicit none'//new_line('a')// &
            'character(len=512) :: marker'//new_line('a')// &
            'character(len=256) :: arg'//new_line('a')// &
            'integer :: unit, i'//new_line('a')// &
            'call get_environment_variable("FO_EXEC_TEST_MARKER", marker)'// &
            new_line('a')// &
            'open(newunit=unit, file=trim(marker), status="unknown", &'// &
            new_line('a')//'    position="append")'//new_line('a')// &
            'write(unit, "(a)") "run"'//new_line('a')//'close(unit)'// &
            new_line('a')//'print "(i0)", command_argument_count()'// &
            new_line('a')//'do i = 1, command_argument_count()'//new_line('a')// &
            '    call get_command_argument(i, arg)'//new_line('a')// &
            '    print "(a)", trim(arg)'//new_line('a')//'end do'// &
            new_line('a')//'end program selected_test'//new_line('a')
    end function selected_source

    function wrapper_source() result(text)
        character(:), allocatable :: text
        text = '#include <stdio.h>'//new_line('a')// &
            '#include <stdlib.h>'//new_line('a')// &
            '#include <unistd.h>'//new_line('a')// &
            'int main(int argc, char **argv) {'//new_line('a')// &
            '    const char *real = getenv("FO_TEST_REAL_COMPILER");'// &
            new_line('a')// &
            '    const char *log = getenv("FO_TEST_COMPILER_LOG");'// &
            new_line('a')// &
            '    FILE *stream = log ? fopen(log, "a") : NULL;'//new_line('a')// &
            '    if (!real || !stream) return 125;'//new_line('a')// &
            '    for (int i = 1; i < argc; ++i) {'//new_line('a')// &
            '        if (fprintf(stream, "%s\n", argv[i]) < 0) return 125;'// &
            new_line('a')//'    }'//new_line('a')// &
            '    if (fclose(stream) != 0) return 125;'//new_line('a')// &
            '    argv[0] = (char *)real;'//new_line('a')// &
            '    execv(real, argv);'//new_line('a')//'    return 127;'// &
            new_line('a')//'}'//new_line('a')
    end function wrapper_source

end program test_exec_selected_test_cli
