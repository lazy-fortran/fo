program test_gremlin_context_provenance
    use, intrinsic :: iso_c_binding, only: c_char, c_int, c_null_char, c_null_ptr
    use fo_test_harness, only: string_list_t, process_result_t, list_add, list_with_first
    use fo_test_harness, only: run_process
    use fo_test_harness, only: make_directory, write_text, read_text
    use fo_test_harness, only: assert_true, assert_equal_string, finish_assertions
    use fo_test_gremlin_oracle, only: gremlin_setup, gremlin_run, gremlin_json
    use fo_test_gremlin_oracle, only: gremlin_start_args, gremlin_wait_ms
    use fo_test_json, only: json_value_t, json_member
    use fo_test_json, only: json_string_value, json_parse, json_number_value
    use fo_test_gremlin_oracle, only: gremlin_stop_lane
    implicit none

    character(:), allocatable :: driver, scratch, project, dependency, cache, state
    character(:), allocatable :: session, generation, previous, identity, initial_identity
    character(:), allocatable :: initial_head, latest_head, compiler, compiler_dir
    character(:), allocatable :: lane, path_value, toolchain_value, counter, calls
    character(:), allocatable :: independent_head, before_identity, probes
    integer :: baseline, repeat
    type(process_result_t) :: process
    type(json_value_t) :: report
    integer :: path_length, path_status

    interface
        integer(c_int) function c_utime(path, times) bind(C, name='utime')
            use, intrinsic :: iso_c_binding, only: c_char, c_int, c_ptr
            character(kind=c_char), intent(in) :: path(*)
            type(c_ptr), value :: times
        end function c_utime
    end interface

    call gremlin_setup(driver, scratch, project, cache, state, counter)
    call get_environment_variable('PATH', length=path_length, status=path_status)
    call assert_true(path_status == 0 .and. path_length > 0, 'fixture has a process PATH')
    allocate(character(len=path_length) :: path_value)
    call get_environment_variable('PATH', path_value)
    dependency = scratch//'/dependency'
    call make_directory(dependency//'/src')
    call write_text(project//'/fpm.toml', &
        'name = "provenance_probe"'//new_line('a')// &
        '[dependencies]'//new_line('a')// &
        'provenance_dep = { path = "../dependency" }'//new_line('a'))
    call write_text(project//'/test/test_provenance.f90', &
        'program test_provenance'//new_line('a')// &
        'use provenance_dep, only: dep_value'//new_line('a')// &
        'implicit none'//new_line('a')// &
        'if (dep_value < 1) error stop 1'//new_line('a')// &
        'end program test_provenance'//new_line('a'))
    call write_text(dependency//'/fpm.toml', 'name = "provenance_dep"'//new_line('a'))
    call write_text(dependency//'/src/provenance_dep.f90', &
        'module provenance_dep'//new_line('a')// &
        'integer, parameter :: dep_value = 1'//new_line('a')// &
        'end module provenance_dep'//new_line('a'))
    call initialize_git(project)
    call git_command(project, [character(len=32) :: 'git', 'rev-parse', 'HEAD'], independent_head)
    call locate_compiler(compiler)
    compiler_dir = scratch//'/compiler-probe-bin'
    calls = scratch//'/compiler.calls'
    call make_directory(compiler_dir)
    call write_compiler_wrapper(compiler, compiler_dir, calls)
    lane = 'context-provenance'
    call start_lane(lane, 'PATH='//compiler_dir//':'//path_value, report)
    session = field(report, 'session_id')
    call assert_true(len(session) > 0, 'provenance start returns an owner')
    call wait_generation(session, lane, '', generation)
    identity = state//'/fo/gremlin/generations/'//generation//'/identity.txt'
    initial_identity = read_text(identity)
    initial_head = line_value(initial_identity, 'base_commit=')
    call assert_equal_string(initial_head//new_line('a'), independent_head, &
        'generation base equals independently queried Git HEAD')
    call assert_true(index(initial_identity, 'input=dependency:provenance_dep|') > 0, &
        'identity includes the path dependency as a generation input')
    call assert_equal_string(line_value(initial_identity, 'toolchain='), &
        compiler_dir//'/gfortran:PROVENANCE_COMPILER_ORACLE', &
        'toolchain records only the independently controlled compiler version probe')
    do repeat = 1, 2
        baseline = capture_count()
        call touch_directory(project)
        call wait_capture(baseline)
        call assert_equal_string(read_text(identity), initial_identity, &
            'unchanged metadata captures preserve the complete generation identity')
    end do

    ! A project source edit changes the captured generation, then a dependency edit
    ! independently changes its path-dependency input digest.
    previous = generation
    call write_text(project//'/test/test_provenance.f90', &
        'program test_provenance'//new_line('a')// &
        'use provenance_dep, only: dep_value'//new_line('a')// &
        'implicit none'//new_line('a')// &
        '! project input variant'//new_line('a')// &
        'if (dep_value < 1) error stop 1'//new_line('a')// &
        'end program test_provenance'//new_line('a'))
    call wait_generation(session, lane, previous, generation)
    call assert_true(generation /= previous, 'project source variation changes generation identity')
    identity = state//'/fo/gremlin/generations/'//generation//'/identity.txt'
    before_identity = read_text(identity)
    previous = generation
    call write_text(dependency//'/src/provenance_dep.f90', &
        'module provenance_dep'//new_line('a')// &
        'integer, parameter :: dep_value = 2'//new_line('a')// &
        'end module provenance_dep'//new_line('a'))
    call wait_generation(session, lane, previous, generation)
    identity = state//'/fo/gremlin/generations/'//generation//'/identity.txt'
    call assert_true(index(read_text(identity), 'input=dependency:provenance_dep|') > 0, &
        'changed path dependency remains in complete identity')
    call assert_true(line_value(read_text(identity), 'input=dependency:provenance_dep|') /= &
        line_value(before_identity, 'input=dependency:provenance_dep|'), &
        'dependency bytes independently change its captured digest')

    ! A metadata-only commit changes the independently measured Git base while source
    ! inputs and diff stay constant. A following uncommitted edit changes patch digest.
    before_identity = read_text(identity)
    call git_command(project, [character(len=32) :: &
        'git', 'commit', '--allow-empty', '-qm', 'metadata only'])
    call git_command(project, [character(len=32) :: 'git', 'rev-parse', 'HEAD'], independent_head)
    call touch_directory(project)
    previous = generation
    call wait_generation(session, lane, previous, generation)
    identity = state//'/fo/gremlin/generations/'//generation//'/identity.txt'
    latest_head = line_value(read_text(identity), 'base_commit=')
    call assert_equal_string(latest_head//new_line('a'), independent_head, &
        'metadata commit base equals independent Git HEAD')
    call assert_true(latest_head /= initial_head, 'metadata commit changes the Git base')
    call assert_equal_string(line_value(read_text(identity), 'patch_digest='), &
        line_value(before_identity, 'patch_digest='), 'metadata commit preserves source diff digest')
    previous = generation
    call write_text(project//'/test/test_provenance.f90', &
        read_text(project//'/test/test_provenance.f90')//'! dirty patch variant'//new_line('a'))
    call wait_generation(session, lane, previous, generation)
    identity = state//'/fo/gremlin/generations/'//generation//'/identity.txt'
    call assert_true(line_value(read_text(identity), 'patch_digest=') /= &
        line_value(initial_identity, 'patch_digest='), 'uncommitted patch input changes patch digest')
    call stop_lane(lane, session)
    probes = read_text(calls)
    call assert_probe_arguments(probes)

    ! Environment inputs produce a different immutable generation on an identical
    ! project tree. Use OMP_NUM_THREADS because it is part of the documented closure.
    call start_lane('context-env-one', 'OMP_NUM_THREADS=1', report)
    session = field(report, 'session_id')
    call wait_generation(session, 'context-env-one', '', generation)
    initial_identity = read_text(state//'/fo/gremlin/generations/'//generation//'/identity.txt')
    call stop_lane('context-env-one', session)
    call start_lane('context-env-two', 'OMP_NUM_THREADS=2', report)
    session = field(report, 'session_id')
    call wait_generation(session, 'context-env-two', '', generation)
    identity = read_text(state//'/fo/gremlin/generations/'//generation//'/identity.txt')
    call assert_true(line_value(identity, 'environment=') /= &
        line_value(initial_identity, 'environment='), 'environment variation is captured: '// &
        line_value(identity, 'environment=')//' versus '// &
        line_value(initial_identity, 'environment='))
    call stop_lane('context-env-two', session)

    ! A distinct compiler path, resolving to the same real gfortran, must still be
    ! represented as a different toolchain identity.
    call locate_compiler(compiler)
    compiler_dir = scratch//'/compiler-bin'
    call make_directory(compiler_dir)
    call write_compiler_wrapper(compiler, compiler_dir, calls)
    call start_lane('context-compiler-one', '', report)
    session = field(report, 'session_id')
    call wait_generation(session, 'context-compiler-one', '', generation)
    initial_identity = read_text(state//'/fo/gremlin/generations/'//generation//'/identity.txt')
    call stop_lane('context-compiler-one', session)
    call start_lane('context-compiler-two', 'PATH='//compiler_dir//':'//path_value, report)
    session = field(report, 'session_id')
    call wait_generation(session, 'context-compiler-two', '', generation)
    identity = read_text(state//'/fo/gremlin/generations/'//generation//'/identity.txt')
    toolchain_value = line_value(identity, 'toolchain=')
    call assert_true(toolchain_value /= line_value(initial_identity, 'toolchain='), &
        'different resolved compiler path changes toolchain provenance: '//toolchain_value// &
        ' versus '//line_value(initial_identity, 'toolchain='))
    call stop_lane('context-compiler-two', session)
    call finish_assertions()

contains

    subroutine write_compiler_wrapper(real_compiler, directory, calls_path)
        character(len=*), intent(in) :: real_compiler, directory, calls_path
        character(:), allocatable :: source
        type(string_list_t) :: args
        type(process_result_t) :: result
        source = '#include <stdio.h>'//new_line('a')// &
            '#include <string.h>'//new_line('a')//'#include <unistd.h>'//new_line('a')// &
            'int main(int argc,char **argv){ char cwd[4096];'//new_line('a')// &
            'if(!getcwd(cwd,sizeof(cwd)))return 120;'//new_line('a')// &
            'FILE *f=fopen("'//calls_path//'","a"); if(!f)return 121;'//new_line('a')// &
            'fprintf(f,"%s",cwd); for(int i=1;i<argc;i++)fprintf(f,"|%s",argv[i]);'// &
            new_line('a')//'fputc(10,f); fclose(f);'//new_line('a')// &
            'if(argc==2 && strcmp(argv[1],"--version")==0){'//new_line('a')// &
            'puts("PROVENANCE_COMPILER_ORACLE");return 0;}'//new_line('a')// &
            'argv[0]="'//real_compiler//'";'//new_line('a')// &
            'execv(argv[0],argv);return 127;}'//new_line('a')
        call write_text(scratch//'/compiler-wrapper.c', source)
        call list_add(args, 'cc')
        call list_add(args, '-o')
        call list_add(args, directory//'/gfortran')
        call list_add(args, scratch//'/compiler-wrapper.c')
        call run_process(args, scratch, result, timeout_ms=30000)
        call assert_true(result%exit_code == 0, 'compiles the OS compiler-probe wrapper')
    end subroutine write_compiler_wrapper

    integer function capture_count() result(count)
        type(json_value_t) :: value, item
        logical :: valid
        character(:), allocatable :: message
        call json_parse(read_text(counter), value, valid, message)
        call assert_true(valid, 'capture observer publishes a complete JSON record')
        item = json_member(value, 'count')
        count = int(json_number_value(item))
    end function capture_count

    subroutine wait_capture(before)
        integer, intent(in) :: before
        integer :: attempt
        do attempt = 1, 300
            if (capture_count() > before) return
            call gremlin_wait_ms(50)
        end do
        call assert_true(.false., 'metadata event completes an independent capture')
    end subroutine wait_capture

    subroutine assert_probe_arguments(text)
        character(len=*), intent(in) :: text
        integer :: first, last, count
        character(:), allocatable :: row
        first = 1
        count = 0
        do while (first <= len(text))
            last = index(text(first:), new_line('a'))
            if (last == 0) exit
            row = text(first:first + last - 2)
            if (index(row, project//'|') == 1) then
                count = count + 1
                call assert_equal_string(row, project//'|--version', &
                    'Git arguments never enter a compiler invocation')
            end if
            first = first + last
        end do
        call assert_true(count >= 3, 'initial and repeated captures independently probe the compiler')
    end subroutine assert_probe_arguments

    function field(document, key) result(value)
        type(json_value_t), intent(in) :: document
        character(len=*), intent(in) :: key
        character(:), allocatable :: value
        type(json_value_t) :: item
        item = json_member(document, key)
        value = json_string_value(item)
    end function field

    function line_value(text, prefix) result(value)
        character(len=*), intent(in) :: text, prefix
        character(:), allocatable :: value
        integer :: first, last
        first = index(text, prefix)
        value = ''
        if (first == 0) return
        first = first + len(prefix)
        last = index(text(first:), new_line('a'))
        if (last == 0) then
            value = text(first:)
        else
            value = text(first:first + last - 2)
        end if
    end function line_value

    subroutine start_lane(lane_id, override, value)
        character(len=*), intent(in) :: lane_id, override
        type(json_value_t), intent(out) :: value
        type(string_list_t) :: args, extra
        type(process_result_t) :: result
        logical :: valid
        character(:), allocatable :: message
        call gremlin_start_args(args, project, lane_id, 'test_provenance')
        call list_add(args, '--json')
        if (len(override) > 0) call list_add(extra, override)
        call gremlin_run(driver, project, cache, state, args, result, extra, 30000)
        call assert_true(result%exit_code == 0, 'starts provenance session '//lane_id)
        call json_parse(result%stdout, value, valid, message)
        call assert_true(valid, 'Gremlin start returns JSON: '//message)
    end subroutine start_lane

    subroutine wait_generation(owner, lane_id, old_generation, current_generation)
        character(len=*), intent(in) :: owner, lane_id, old_generation
        character(:), allocatable, intent(out) :: current_generation
        type(json_value_t) :: value
        type(string_list_t) :: args
        type(process_result_t) :: result
        integer :: attempt
        current_generation = ''
        do attempt = 1, 300
            call list_add(args, 'gremlin')
            call list_add(args, 'status')
            call list_add(args, '--dir')
            call list_add(args, project)
            call list_add(args, '--lane')
            call list_add(args, lane_id)
            call list_add(args, '--session')
            call list_add(args, owner)
            call list_add(args, '--json')
            call gremlin_json(driver, project, cache, state, args, value, result, 30000)
            current_generation = field(value, 'active_generation')
            if (len(current_generation) == 64 .and. current_generation /= old_generation) return
            call gremlin_wait_ms(100)
            args = string_list_t()
        end do
        call assert_true(.false., 'capture publishes a new active generation')
    end subroutine wait_generation

    subroutine stop_lane(lane_id, owner)
        character(len=*), intent(in) :: lane_id, owner
        call gremlin_stop_lane(driver, project, cache, state, lane_id, owner)
    end subroutine stop_lane

    subroutine initialize_git(dir)
        character(len=*), intent(in) :: dir
        call git_command(dir, [character(len=32) :: 'git', 'init', '-q'])
        call git_command(dir, [character(len=32) :: 'git', 'config', 'user.name', 'Fortran Oracle'])
        call git_command(dir, [character(len=32) :: 'git', 'config', 'user.email', &
            'oracle@example.invalid'])
        call git_command(dir, [character(len=32) :: 'git', 'add', 'fpm.toml', 'test'])
        call git_command(dir, [character(len=32) :: 'git', 'commit', '-qm', 'initial fixture'])
    end subroutine initialize_git

    subroutine git_command(dir, argv, output)
        character(len=*), intent(in) :: dir
        character(len=*), intent(in) :: argv(:)
        character(:), allocatable, optional, intent(out) :: output
        type(string_list_t) :: args, command
        type(process_result_t) :: result
        integer :: i
        do i = 1, size(argv)
            call list_add(args, trim(argv(i)))
        end do
        call run_process(args, dir, result, timeout_ms=30000)
        call assert_true(result%exit_code == 0, 'independent Git oracle command succeeds')
        if (present(output)) output = result%stdout
    end subroutine git_command

    subroutine touch_directory(dir)
        character(len=*), intent(in) :: dir
        integer(c_int) :: result
        ! The watcher treats project-directory metadata as a relevant event.
        result = c_utime(trim(dir)//c_null_char, c_null_ptr)
        call assert_true(result == 0, 'emits a real directory metadata event')
        call gremlin_wait_ms(50)
    end subroutine touch_directory

    subroutine locate_compiler(path)
        character(:), allocatable, intent(out) :: path
        type(string_list_t) :: args, command
        type(process_result_t) :: result
        call list_add(args, 'gfortran')
        call list_with_first('which', args, command)
        call run_process(command, project, result, timeout_ms=10000)
        call assert_true(result%exit_code == 0, 'locates the real compiler')
        path = result%stdout(:index(result%stdout, new_line('a')) - 1)
    end subroutine locate_compiler

end program test_gremlin_context_provenance
