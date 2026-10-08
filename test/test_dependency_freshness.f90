program test_dependency_freshness
    !! Path-dependency inputs reach the executable that `fo exec` runs next.
    !! Each scenario builds once, changes or removes one dependency input and
    !! checks the observable output or exit status of the public command.
    use fo_test_harness, only: process_result_t, string_list_t, list_add
    use fo_test_harness, only: make_scratch, join_path, write_text, write_lines
    use fo_test_harness, only: read_text, environment_value
    use fo_test_harness, only: remove_tree, remove_path, assert_process_ok
    use fo_test_harness, only: file_exists, assert_file_equals
    use fo_test_harness, only: assert_equal_string, assert_true, assert_not_contains
    use fo_test_harness, only: assert_contains
    use fo_fs, only: fs_stat, fs_find_executable
    use, intrinsic :: iso_c_binding, only: c_long_long
    use fo_test_harness, only: finish_assertions
    use fo_test_cli, only: resolve_driver, run_fo, run_external
    implicit none

    character(:), allocatable :: driver, scratch, cache
    character(len=512) :: chmod_tool, copy_tool, touch_tool
    character(len=1), parameter :: nl = new_line('a')
    type(process_result_t) :: result
    type(string_list_t) :: arguments, environment

    call resolve_driver(driver)
    call require_utility('chmod', chmod_tool)
    call require_utility('cp', copy_tool)
    call require_utility('touch', touch_tool)
    call make_scratch('fo-dependency-freshness-cli', scratch)
    cache = join_path(scratch, 'cache')

    call edit_same_size_source()
    call remove_build_named_source()
    call remove_module_file()
    call edit_c_header()
    call edit_fortran_include()
    call add_dependency_source()
    call change_flags_and_tool()
    call change_c_tool()
    call switch_manifest_source_dir()

    call remove_tree(scratch)
    call finish_assertions()
    write (*, '(a)') &
        'dependency-freshness-cli: source, include, header, module, flags, tool'

contains

    subroutine require_utility(name, path)
        character(len=*), intent(in) :: name
        character(len=*), intent(out) :: path
        logical :: found

        call fs_find_executable(name, path, found)
        call assert_true(found, 'freshness oracle requires executable '//name)
        if (.not. found) call finish_assertions()
    end subroutine require_utility

    subroutine assert_public_ok(message)
        character(len=*), intent(in) :: message

        call assert_process_ok(result, message)
        if (result%exit_code == 0) return
        write (*, '(a)') trim(message)//': '// &
            result%stderr(:min(len(result%stderr), 4000))
    end subroutine assert_public_ok

    subroutine edit_same_size_source()
        !! A private body edit changes runtime and tests while preserving its interface.
        character(:), allocatable :: consumer, dep, source, reference
        character(:), allocatable :: compiler, count, before, after
        integer(c_long_long) :: changed_mtime, changed_size
        integer(c_long_long) :: source_mtime, source_size
        logical :: ok

        compiler = join_path(scratch, 'gfortran-private-wrapper')
        count = compiler // '.count'
        call write_tool(compiler, count, '61')
        call setup_build_named('edit', consumer, dep, compiler)
        source = join_path(dep, 'src/build_info.f90')
        reference = join_path(scratch, 'source.reference')
        call write_text(join_path(consumer, 'test/check_dependency.f90'), &
            'program check_dependency' // nl // &
            'use build_info, only: build_value' // nl // &
            "if (build_value() /= 11) error stop 'expected dependency v1 value 11'" // &
            nl // 'end program check_dependency' // nl)
        call test_consumer(consumer)
        call assert_public_ok('independent dependency v1 assertion passes')
        before = read_text(count)
        call fs_stat(source, source_mtime, source_size, ok)
        call assert_true(ok, 'initial dependency source can be statted')
        call preserve_metadata(source, reference, .false.)
        call write_build_info(source, 22)
        call preserve_metadata(source, reference, .true.)
        call fs_stat(source, changed_mtime, changed_size, ok)
        call assert_true(ok, 'edited dependency source can be statted')
        call assert_true(changed_mtime == source_mtime .and. &
            changed_size == source_size, 'same-size edit retains original source mtime')
        call exec_target(consumer, 'consumer', compiler=compiler)
        call assert_public_ok('timestamp-restored dependency builds')
        call assert_equal_string(result%stdout, '22' // nl, &
            'timestamp-restored private source edit reaches runtime')
        after = read_text(count)
        call assert_true(len(after) >= len(before), &
            'compiler observations retain history')
        if (len(after) >= len(before)) then
            call assert_not_contains(after(len(before) + 1:), 'app/main.f90', &
                'private dependency edit preserves dependent compilation')
        end if
        call test_consumer(consumer)
        call assert_true(result%exit_code /= 0, &
            'dependency v1 assertion fails after edit')
        call assert_contains(result%stdout, 'expected dependency v1 value 11', &
            'post-edit failure comes from the independent runtime assertion')
    end subroutine edit_same_size_source

    subroutine test_consumer(consumer)
        character(len=*), intent(in) :: consumer

        arguments = string_list_t()
        call list_add(arguments, 'test')
        call list_add(arguments, '--json')
        call run_fo(driver, arguments, consumer, cache, result, environment)
    end subroutine test_consumer

    subroutine remove_build_named_source()
        !! Deleting such a source must not let the warm executable keep running.
        character(:), allocatable :: consumer, dep

        call setup_build_named('remove', consumer, dep)
        call remove_path(join_path(dep, 'src/build_info.f90'))
        call exec_target(consumer, 'consumer')
        call assert_true(result%exit_code /= 0, &
            'deleted build-named dependency source fails the warm build')
        call assert_not_contains(result%stdout, '11', &
            'deleted build-named dependency source runs no stale executable')
    end subroutine remove_build_named_source

    subroutine remove_module_file()
        !! A missing dependency module file is regenerated before linking.
        character(:), allocatable :: consumer, dep

        call setup_build_named('module', consumer, dep)
        call remove_path(join_path(consumer, 'build/fo/mod/build_info.mod'))
        call exec_target(consumer, 'consumer')
        call assert_public_ok('missing dependency module file is rebuilt')
        call assert_true(file_exists( &
            join_path(consumer, 'build/fo/mod/build_info.mod')), &
            'dependency module interface is restored')
        call assert_equal_string(result%stdout, '11' // nl, &
            'rebuilt dependency module keeps the executable behavior')
    end subroutine remove_module_file

    subroutine edit_c_header()
        !! A header in a path dependency include directory invalidates its C object.
        character(:), allocatable :: consumer, dep, header, c_source

        consumer = join_path(scratch, 'c-header/consumer')
        dep = join_path(scratch, 'c-header/cdep')
        header = join_path(dep, 'include/cdep_value.h')
        c_source = join_path(dep, 'src/cdep_value.c')
        call write_consumer(consumer, 'cdep', '../cdep')
        call write_c_main(consumer)
        call write_text(join_path(dep, 'fpm.toml'), 'name = "cdep"' // nl)
        call write_text(join_path(dep, 'src/cdep.f90'), &
            'module cdep' // nl // 'implicit none' // nl // 'end module cdep' // nl)
        call write_text(c_source, &
            '#include "cdep_value.h"' // nl // &
            'int dep_c_value(void) { return CDEP_VALUE * 1; }' // nl)
        call write_text(header, '#define CDEP_VALUE 21' // nl)
        call exec_target(consumer, 'consumer')
        call assert_public_ok('C dependency with include header builds')
        call assert_equal_string(result%stdout, '21' // nl, &
            'initial C dependency header value is linked')
        call preserve_metadata(header, join_path(scratch, 'header.reference'), .false.)
        call write_text(header, '#define CDEP_VALUE 27' // nl)
        call preserve_metadata(header, join_path(scratch, 'header.reference'), .true.)
        call exec_target(consumer, 'consumer')
        call assert_public_ok('edited C dependency header builds')
        call assert_equal_string(result%stdout, '27' // nl, &
            'timestamp-restored C header edit reaches the executable')
        call preserve_metadata(c_source, &
            join_path(scratch, 'c-source.reference'), .false.)
        call write_text(c_source, '#include "cdep_value.h"' // nl // &
            'int dep_c_value(void) { return CDEP_VALUE * 2; }' // nl)
        call preserve_metadata(c_source, &
            join_path(scratch, 'c-source.reference'), .true.)
        call exec_target(consumer, 'consumer')
        call assert_public_ok('edited C dependency source builds')
        call assert_equal_string(result%stdout, '54' // nl, &
            'timestamp-restored C source edit reaches the executable')
    end subroutine edit_c_header

    subroutine edit_fortran_include()
        character(:), allocatable :: consumer, dep, include

        call setup_build_named('include', consumer, dep)
        include = join_path(dep, 'include/value.inc')
        call write_text(include, 'build_value = 31' // nl)
        call write_text(join_path(dep, 'src/build_info.f90'), &
            'module build_info' // nl // 'contains' // nl // &
            'integer function build_value()' // nl // &
            "include 'value.inc'" // nl // &
            'end function build_value' // nl // 'end module build_info' // nl)
        call exec_target(consumer, 'consumer')
        call assert_public_ok('Fortran dependency include builds')
        call assert_equal_string(result%stdout, '31' // nl, 'initial include value')
        call preserve_metadata(include, &
            join_path(scratch, 'include.reference'), .false.)
        call write_text(include, 'build_value = 32' // nl)
        call preserve_metadata(include, &
            join_path(scratch, 'include.reference'), .true.)
        call exec_target(consumer, 'consumer')
        call assert_public_ok('edited Fortran dependency include builds')
        call assert_equal_string(result%stdout, '32' // nl, &
            'Fortran include edit reaches the executable')
    end subroutine edit_fortran_include

    subroutine add_dependency_source()
        character(:), allocatable :: consumer, dep, extra

        call setup_build_named('add-source', consumer, dep)
        extra = join_path(dep, 'src/extra.f90')
        call write_text(extra, 'integer function extra_value()' // nl // &
            'extra_value = 47' // nl // 'end function extra_value' // nl)
        call write_text(join_path(dep, 'src/build_info.f90'), &
            'module build_info' // nl // 'contains' // nl // &
            'integer function build_value()' // nl // &
            'integer, external :: extra_value' // nl // &
            'build_value = extra_value()' // nl // &
            'end function build_value' // nl // 'end module build_info' // nl)
        call exec_target(consumer, 'consumer')
        call assert_public_ok('new dependency source is compiled and linked')
        call assert_equal_string(result%stdout, '47' // nl, &
            'new source reaches runtime')
        call remove_path(extra)
        call exec_target(consumer, 'consumer')
        call assert_true(result%exit_code /= 0, 'deleted external source fails link')
        call assert_not_contains(result%stdout, '47', 'removed object is not reused')
    end subroutine add_dependency_source

    subroutine change_flags_and_tool()
        character(:), allocatable :: consumer, dep, compiler, reference, count, before

        call setup_build_named('tool', consumer, dep)
        compiler = join_path(scratch, 'gfortran-wrapper')
        reference = compiler // '.reference'
        count = compiler // '.count'
        call write_text(join_path(dep, 'src/build_info.f90'), &
            'module build_info' // nl // 'contains' // nl // &
            'integer function build_value()' // nl // &
            '#ifdef DEPENDENCY_FLAG' // nl // 'build_value = 99' // nl // &
            '#else' // nl // 'build_value = DEPENDENCY_VALUE' // nl // &
            '#endif' // nl // 'end function build_value' // nl // &
            'end module build_info' // nl)
        call write_tool(compiler, count, '61', version=repeat('v', 600))
        call exec_target(consumer, 'consumer', compiler=compiler)
        call assert_public_ok('dependency builds through selected compiler')
        call assert_equal_string(result%stdout, '61' // nl, 'compiler-defined value')
        before = read_text(count)
        call exec_target(consumer, 'consumer', compiler=compiler)
        call assert_public_ok('unchanged compiler warm build succeeds')
        call assert_file_equals(count, before, 'warm dependency compiles no sources')
        call preserve_metadata(compiler, reference, .false.)
        call write_tool(compiler, count, '62', version=repeat('v', 600))
        call preserve_metadata(compiler, reference, .true.)
        call exec_target(consumer, 'consumer', compiler=compiler)
        call assert_public_ok('edited compiler warm build succeeds')
        call assert_equal_string(result%stdout, '62' // nl, &
            'same-size timestamp-restored compiler edit changes runtime')
        call exec_target(consumer, 'consumer', '-DDEPENDENCY_FLAG', compiler)
        call assert_public_ok('changed dependency flag builds')
        call assert_equal_string(result%stdout, '99' // nl, &
            'changed flag changes runtime')
        call exec_target(consumer, 'consumer', compiler=compiler)
        call assert_public_ok('restored dependency flags build')
        call assert_equal_string(result%stdout, '62' // nl, &
            'restored flags select runtime')
    end subroutine change_flags_and_tool

    subroutine change_c_tool()
        character(:), allocatable :: consumer, dep, compiler, count, before, path
        character(len=512) :: gcc
        logical :: found

        consumer = join_path(scratch, 'c-tool/consumer')
        dep = join_path(scratch, 'c-tool/cdep')
        compiler = join_path(scratch, 'c-bin/gcc')
        count = compiler // '.count'
        call fs_find_executable('gcc', gcc, found)
        call assert_true(found, 'resolve actual C compiler for independent wrapper')
        if (.not. found) return
        call write_consumer(consumer, 'cdep', '../cdep')
        call write_c_main(consumer)
        call write_text(join_path(dep, 'fpm.toml'), 'name = "cdep"' // nl)
        call write_text(join_path(dep, 'src/cdep.f90'), &
            'module cdep' // nl // 'end module cdep' // nl)
        call write_text(join_path(dep, 'src/cdep_value.c'), &
            'int dep_c_value(void) { return C_TOOL_VALUE; }' // nl)
        call write_tool(compiler, count, '71', trim(gcc), 'C_TOOL_VALUE')
        path = join_path(scratch, 'c-bin') // ':' // environment_value('PATH')
        call exec_target(consumer, 'consumer', path=path)
        call assert_public_ok('dependency builds through selected C compiler')
        call assert_equal_string(result%stdout, '71' // nl, 'C compiler-defined value')
        before = read_text(count)
        call exec_target(consumer, 'consumer', path=path)
        call assert_public_ok('unchanged C compiler warm build succeeds')
        call assert_file_equals(count, before, 'warm C dependency compiles no sources')
        call preserve_metadata(compiler, compiler // '.reference', .false.)
        call write_tool(compiler, count, '72', trim(gcc), 'C_TOOL_VALUE')
        call preserve_metadata(compiler, compiler // '.reference', .true.)
        call exec_target(consumer, 'consumer', path=path)
        call assert_public_ok('edited C compiler warm build succeeds')
        call assert_equal_string(result%stdout, '72' // nl, &
            'timestamp-restored C compiler edit changes runtime')
    end subroutine change_c_tool

    subroutine write_tool(path, count, value, compiler, definition, version)
        character(len=*), intent(in) :: path, count, value
        character(len=*), optional, intent(in) :: compiler, definition, version
        character(:), allocatable :: command_name, macro, version_probe
        type(string_list_t) :: command
        type(process_result_t) :: child

        command_name = 'gfortran'
        macro = 'DEPENDENCY_VALUE'
        if (present(compiler)) command_name = compiler
        if (present(definition)) macro = definition
        version_probe = ''
        if (present(version)) version_probe = &
            'if [ "$1" = --version ]; then printf "%s\n" "' // &
            version // '"; exit 0; fi' // nl
        call write_text(path, '#!/bin/sh' // nl // version_probe // &
            'for arg in "$@"; do' // nl // &
            '  case "$arg" in ' // &
            '*/src/*.f90|*/app/*.f90|*/test/*.f90|*/src/*.c)' // nl // &
            '    printf "%s\n" "$arg" >> "' // &
            count // '" ;; esac' // nl // &
            'done' // nl // 'exec "' // command_name // '" -D' // macro // '=' // &
            value // ' "$@"' // nl)
        call list_add(command, '+x')
        call list_add(command, path)
        call run_external(trim(chmod_tool), command, scratch, child)
        call assert_process_ok(child, 'make compiler wrapper executable: '//path)
    end subroutine write_tool

    subroutine preserve_metadata(path, reference, restore)
        character(len=*), intent(in) :: path, reference
        logical, intent(in) :: restore
        type(string_list_t) :: command
        type(process_result_t) :: child

        if (restore) then
            call list_add(command, '-r')
            call list_add(command, reference)
            call list_add(command, path)
            call run_external(trim(touch_tool), command, scratch, child)
            call assert_process_ok(child, 'restore input timestamps: '//path)
        else
            call list_add(command, '-p')
            call list_add(command, path)
            call list_add(command, reference)
            call run_external(trim(copy_tool), command, scratch, child)
            call assert_process_ok(child, 'save metadata reference: '//reference)
        end if
    end subroutine preserve_metadata

    subroutine switch_manifest_source_dir()
        !! A dependency manifest that moves its library sources is an input.
        character(:), allocatable :: consumer, dep

        consumer = join_path(scratch, 'manifest/consumer')
        dep = join_path(scratch, 'manifest/buildlib')
        call write_consumer(consumer, 'buildlib', '../buildlib')
        call write_main_build_value(consumer)
        call write_source_dir(dep, 'lib')
        call write_build_info(join_path(dep, 'lib/build_info.f90'), 11)
        call exec_target(consumer, 'consumer')
        call assert_public_ok('library in configured source-dir builds')
        call assert_equal_string(result%stdout, '11' // nl, &
            'initial configured source-dir is linked')
        call write_source_dir(dep, 'src')
        call write_build_info(join_path(dep, 'src/build_info.f90'), 22)
        call exec_target(consumer, 'consumer')
        call assert_public_ok('manifest source-dir change builds')
        call assert_equal_string(result%stdout, '22' // nl, &
            'manifest source-dir change reaches the executable')
    end subroutine switch_manifest_source_dir

    subroutine setup_build_named(name, consumer, dep, compiler)
        !! Builds the consumer once with dependency value 11 in src/build_info.f90.
        character(len=*), intent(in) :: name
        character(len=*), optional, intent(in) :: compiler
        character(:), allocatable, intent(out) :: consumer, dep

        consumer = join_path(scratch, name // '/consumer')
        dep = join_path(scratch, name // '/buildlib')
        call write_consumer(consumer, 'buildlib', '../buildlib')
        call write_main_build_value(consumer)
        call write_text(join_path(dep, 'fpm.toml'), 'name = "buildlib"' // nl)
        call write_build_info(join_path(dep, 'src/build_info.f90'), 11)
        call exec_target(consumer, 'consumer', compiler=compiler)
        call assert_public_ok('build-named dependency builds initially')
        call assert_equal_string(result%stdout, '11' // nl, &
            'initial build-named dependency value is linked')
    end subroutine setup_build_named

    subroutine write_consumer(consumer, dep_name, dep_path)
        character(len=*), intent(in) :: consumer, dep_name, dep_path

        call write_text(join_path(consumer, 'fpm.toml'), &
            'name = "consumer"' // nl // '[dependencies]' // nl // &
            dep_name // ' = { path = "' // dep_path // '" }' // nl)
    end subroutine write_consumer

    subroutine write_main_build_value(consumer)
        character(len=*), intent(in) :: consumer
        character(len=88) :: lines(5)

        lines = [character(len=88) :: 'program main', &
            'use build_info, only: build_value', 'implicit none', &
            "print '(i0)', build_value()", 'end program main']
        call write_lines(join_path(consumer, 'app/main.f90'), lines)
    end subroutine write_main_build_value

    subroutine write_build_info(path, value)
        character(len=*), intent(in) :: path
        integer, intent(in) :: value
        character(len=16) :: text
        character(len=88) :: lines(7)

        write (text, '(i0)') value
        lines = [character(len=88) :: 'module build_info', 'implicit none', &
            'contains', 'integer function build_value()', 'build_value = 0', &
            'end function build_value', 'end module build_info']
        lines(5) = '    build_value = ' // trim(text)
        call write_lines(path, lines)
    end subroutine write_build_info

    subroutine write_source_dir(dep, source_dir)
        character(len=*), intent(in) :: dep, source_dir

        call write_text(join_path(dep, 'fpm.toml'), &
            'name = "buildlib"' // nl // '[library]' // nl // &
            'source-dir = "' // source_dir // '"' // nl)
    end subroutine write_source_dir

    subroutine write_c_main(consumer)
        character(len=*), intent(in) :: consumer
        character(len=88) :: lines(10)

        lines = [character(len=88) :: 'program main', &
            'use, intrinsic :: iso_c_binding, only: c_int', 'implicit none', &
            'interface', &
            "integer(c_int) function dep_c_value() bind(C, name='dep_c_value')", &
            'import :: c_int', 'end function dep_c_value', 'end interface', &
            "print '(i0)', dep_c_value()", 'end program main']
        call write_lines(join_path(consumer, 'app/main.f90'), lines)
    end subroutine write_c_main

    subroutine exec_target(consumer, target, flag, compiler, path)
        character(len=*), intent(in) :: consumer, target
        character(len=*), optional, intent(in) :: flag, compiler, path

        arguments = string_list_t()
        call list_add(arguments, 'exec')
        if (present(flag)) then
            call list_add(arguments, '--flag')
            call list_add(arguments, flag)
        end if
        call list_add(arguments, target)
        environment = string_list_t()
        call list_add(environment, 'TMPDIR=' // scratch)
        if (present(compiler)) call list_add(environment, 'FO_FC=' // compiler)
        if (present(path)) call list_add(environment, 'PATH=' // path)
        call run_fo(driver, arguments, consumer, cache, result, environment)
    end subroutine exec_target

end program test_dependency_freshness
