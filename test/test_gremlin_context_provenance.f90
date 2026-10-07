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
    use fo_test_json, only: json_element, json_size, json_boolean_value
    use fo_test_gremlin_oracle, only: gremlin_stop_lane
    use fo_generation_manifest, only: generation_manifest_metadata_t, &
        generation_manifest_load
    use fo_input_inventory, only: input_inventory_t
    implicit none

    character(:), allocatable :: driver, scratch, project, dependency, cache, state
    character(:), allocatable :: session, generation, previous, compiler, compiler_dir
    character(:), allocatable :: lane, path_value, counter, calls
    character(:), allocatable :: independent_head, probes
    character(len=512) :: manifest_message
    type(generation_manifest_metadata_t) :: initial_metadata, metadata, before_metadata
    type(input_inventory_t) :: initial_inventory, inventory, before_inventory
    integer :: baseline, repeat
    type(process_result_t) :: process
    type(json_value_t) :: report, before_metadata_pass_receipt, retained_pass_receipt
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
    call read_generation_manifest(generation, initial_metadata, initial_inventory)
    call assert_equal_string(initial_metadata%base_commit//new_line('a'), independent_head, &
        'generation base equals independently queried Git HEAD')
    call assert_true(len(input_digest(initial_inventory, &
        'dependency:provenance_dep', 'src/provenance_dep.f90')) == 64, &
        'identity includes the path dependency as a generation input')
    call assert_equal_string(initial_metadata%toolchain, &
        compiler_dir//'/gfortran:PROVENANCE_COMPILER_ORACLE', &
        'toolchain records only the independently controlled compiler version probe')
    do repeat = 1, 2
        baseline = capture_count()
        call touch_directory(project)
        call wait_capture(baseline)
        call read_generation_manifest(generation, metadata, inventory)
        call assert_equal_string(metadata%execution_identity, &
            initial_metadata%execution_identity, &
            'unchanged metadata captures preserve the complete generation identity')
        call assert_equal_string(inventory%digest, initial_inventory%digest, &
            'unchanged metadata captures preserve the input inventory')
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
    call read_generation_manifest(generation, before_metadata, before_inventory)
    previous = generation
    call write_text(dependency//'/src/provenance_dep.f90', &
        'module provenance_dep'//new_line('a')// &
        'integer, parameter :: dep_value = 2'//new_line('a')// &
        'end module provenance_dep'//new_line('a'))
    call wait_generation(session, lane, previous, generation)
    call read_generation_manifest(generation, metadata, inventory)
    call assert_true(len(input_digest(inventory, 'dependency:provenance_dep', &
        'src/provenance_dep.f90')) == 64, &
        'changed path dependency remains in complete identity')
    call assert_true(input_digest(inventory, 'dependency:provenance_dep', &
        'src/provenance_dep.f90') /= input_digest(before_inventory, &
        'dependency:provenance_dep', 'src/provenance_dep.f90'), &
        'dependency bytes independently change its captured digest')

    ! A metadata-only commit preserves the execution identity and its original
    ! provenance. A following source edit captures the new Git base and patch.
    before_metadata = metadata
    call wait_pass_receipt(session, lane, generation, before_metadata_pass_receipt)
    call git_command(project, [character(len=32) :: &
        'git', 'commit', '--allow-empty', '-qm', 'metadata only'])
    call git_command(project, [character(len=32) :: 'git', 'rev-parse', 'HEAD'], independent_head)
    baseline = capture_count()
    call touch_directory(project)
    call wait_capture(baseline)
    call assert_equal_string(capture_generation(), generation, &
        'metadata-only commit reuses the execution generation')
    call find_pass_receipt(session, lane, generation, retained_pass_receipt, &
        field(before_metadata_pass_receipt, 'completion_id'))
    call assert_equal_string(field(retained_pass_receipt, 'completion_id'), &
        field(before_metadata_pass_receipt, 'completion_id'), &
        'metadata-only commit retains the same executable PASS evidence')
    call assert_equal_string(field(retained_pass_receipt, 'log_path'), &
        field(before_metadata_pass_receipt, 'log_path'), &
        'retained PASS evidence still names its original execution log')
    call read_generation_manifest(generation, metadata, inventory)
    call assert_equal_string(metadata%base_commit, before_metadata%base_commit, &
        'reused manifest retains its original Git provenance')
    call assert_equal_string(metadata%patch_digest, before_metadata%patch_digest, &
        'metadata commit preserves source diff digest')
    previous = generation
    call write_text(project//'/test/test_provenance.f90', &
        read_text(project//'/test/test_provenance.f90')//'! dirty patch variant'//new_line('a'))
    call wait_generation(session, lane, previous, generation)
    call read_generation_manifest(generation, metadata, inventory)
    call assert_equal_string(metadata%base_commit//new_line('a'), independent_head, &
        'source edit records the independently queried new Git HEAD')
    call assert_true(metadata%base_commit /= before_metadata%base_commit, &
        'source edit advances manifest Git provenance')
    call assert_true(metadata%patch_digest /= before_metadata%patch_digest, &
        'uncommitted patch input changes patch digest')
    call stop_lane(lane, session)
    probes = read_text(calls)
    call assert_probe_arguments(probes)

    ! Environment inputs produce a different immutable generation on an identical
    ! project tree. Keep OMP_NUM_THREADS fixed to isolate LIBRARY_PATH.
    call start_lane('context-env-one', 'OMP_NUM_THREADS=1', report, &
        'LIBRARY_PATH='//scratch//'/lib-one')
    session = field(report, 'session_id')
    call wait_generation(session, 'context-env-one', '', generation)
    call read_generation_manifest(generation, initial_metadata, initial_inventory)
    call assert_true(index(initial_metadata%environment, &
        'LIBRARY_PATH='//scratch//'/lib-one') > 0, &
        'generation records the linker search path')
    call stop_lane('context-env-one', session)
    call start_lane('context-env-two', 'OMP_NUM_THREADS=1', report, &
        'LIBRARY_PATH='//scratch//'/lib-two')
    session = field(report, 'session_id')
    call wait_generation(session, 'context-env-two', '', generation)
    call read_generation_manifest(generation, metadata, inventory)
    call assert_true(metadata%execution_identity /= initial_metadata%execution_identity, &
        'linker search path variation changes the generation identity')
    call assert_true(index(metadata%environment, &
        'LIBRARY_PATH='//scratch//'/lib-two') > 0, &
        'linker search path variation is captured')
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
    call read_generation_manifest(generation, before_metadata, before_inventory)
    call stop_lane('context-compiler-one', session)
    call start_lane('context-compiler-two', 'PATH='//compiler_dir//':'//path_value, report)
    session = field(report, 'session_id')
    call wait_generation(session, 'context-compiler-two', '', generation)
    call read_generation_manifest(generation, metadata, inventory)
    call assert_true(metadata%toolchain /= before_metadata%toolchain, &
        'different resolved compiler path changes toolchain provenance: '//metadata%toolchain// &
        ' versus '//before_metadata%toolchain)
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

    function capture_generation() result(identity)
        character(:), allocatable :: identity
        type(json_value_t) :: value, item
        logical :: valid
        character(:), allocatable :: message

        call json_parse(read_text(counter), value, valid, message)
        call assert_true(valid, 'capture observer publishes valid generation JSON')
        item = json_member(value, 'generation')
        identity = json_string_value(item)
    end function capture_generation

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

    subroutine read_generation_manifest(generation_id, value, input_inventory)
        character(len=*), intent(in) :: generation_id
        type(generation_manifest_metadata_t), intent(out) :: value
        type(input_inventory_t), intent(out) :: input_inventory
        character(:), allocatable :: generation_root, store_root, manifest_id
        integer :: ierr

        generation_root = state//'/fo/gremlin/generations-v2/'//trim(generation_id)
        store_root = first_line(read_text(generation_root//'/store.root'))
        manifest_id = first_line(read_text(generation_root//'/manifest.id'))
        call generation_manifest_load(store_root, manifest_id, value, &
            input_inventory, ierr, manifest_message)
        call assert_true(ierr == 0, 'loads generation manifest: '//trim(manifest_message))
        if (ierr == 0) then
            call assert_equal_string(value%execution_identity, generation_id, &
                'manifest identity matches the public generation identifier')
            call assert_true(input_inventory%complete, &
                'generation manifest contains a complete input inventory')
        end if
    end subroutine read_generation_manifest

    function input_digest(input_inventory, root_alias, relative_path) result(digest)
        type(input_inventory_t), intent(in) :: input_inventory
        character(len=*), intent(in) :: root_alias, relative_path
        character(len=64) :: digest
        integer :: i

        digest = ''
        if (.not. allocated(input_inventory%entries)) return
        do i = 1, input_inventory%entry_count
            if (trim(input_inventory%entries(i)%root_alias) /= trim(root_alias)) cycle
            if (trim(input_inventory%entries(i)%relative_path) /= &
                trim(relative_path)) cycle
            digest = input_inventory%entries(i)%content_digest
            return
        end do
    end function input_digest

    function first_line(text) result(value)
        character(len=*), intent(in) :: text
        character(:), allocatable :: value
        integer :: last

        last = index(text, new_line('a'))
        if (last > 0) then
            value = text(:last - 1)
        else
            value = trim(text)
        end if
    end function first_line

    subroutine start_lane(lane_id, override, value, additional_override)
        character(len=*), intent(in) :: lane_id, override
        type(json_value_t), intent(out) :: value
        character(len=*), intent(in), optional :: additional_override
        type(string_list_t) :: args, extra
        type(process_result_t) :: result
        logical :: valid
        character(:), allocatable :: message
        call gremlin_start_args(args, project, lane_id, 'test_provenance')
        if (len(override) > 0) call list_add(extra, override)
        if (present(additional_override)) then
            if (len_trim(additional_override) > 0) then
                call list_add(extra, additional_override)
            end if
        end if
        call gremlin_run(driver, project, cache, state, args, result, extra, 30000)
        call assert_true(result%exit_code == 0, 'starts provenance session '//lane_id)
        if (result%exit_code /= 0) then
            write (*, '(a,i0)') 'provenance start exit: ', result%exit_code
            if (allocated(result%stderr)) write (*, '(a)') &
                result%stderr(:min(1000, len(result%stderr)))
            if (allocated(result%stdout)) write (*, '(a)') &
                result%stdout(:min(1000, len(result%stdout)))
            error stop 1
        end if
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
            call list_add(args, '--detail')
            call list_add(args, 'full')
            call list_add(args, '--dir')
            call list_add(args, project)
            call list_add(args, '--lane')
            call list_add(args, lane_id)
            call list_add(args, '--session')
            call list_add(args, owner)
            call gremlin_json(driver, project, cache, state, args, value, result, 30000)
            current_generation = field(value, 'active_generation')
            if (len(current_generation) == 64 .and. current_generation /= old_generation) return
            call gremlin_wait_ms(100)
            args = string_list_t()
        end do
        call assert_true(.false., 'capture publishes a new active generation')
    end subroutine wait_generation

    subroutine wait_pass_receipt(owner, lane_id, generation_id, receipt)
        character(len=*), intent(in) :: owner, lane_id, generation_id
        type(json_value_t), intent(out) :: receipt
        integer :: attempt

        do attempt = 1, 300
            call find_pass_receipt(owner, lane_id, generation_id, receipt)
            if (len(field(receipt, 'completion_id')) > 0) return
            call gremlin_wait_ms(100)
        end do
        call assert_true(.false., 'initial generation produces a PASS receipt')
    end subroutine wait_pass_receipt

    subroutine find_pass_receipt(owner, lane_id, generation_id, receipt, completion_id)
        character(len=*), intent(in) :: owner, lane_id, generation_id
        type(json_value_t), intent(out) :: receipt
        character(len=*), optional, intent(in) :: completion_id
        type(json_value_t) :: status, events, event, cursor_value, more_value
        type(string_list_t) :: args
        type(process_result_t) :: result
        integer :: i, page, cursor, next_cursor
        character(len=32) :: cursor_text
        logical :: has_more

        receipt%kind = 0
        cursor = 0
        do page = 1, 256
            args = string_list_t()
            write(cursor_text, '(i0)') cursor
            call list_add(args, 'gremlin')
            call list_add(args, 'status')
            call list_add(args, '--detail')
            call list_add(args, 'full')
            call list_add(args, '--dir')
            call list_add(args, project)
            call list_add(args, '--lane')
            call list_add(args, lane_id)
            call list_add(args, '--session')
            call list_add(args, owner)
            call list_add(args, '--cursor')
            call list_add(args, trim(cursor_text))
            call list_add(args, '--max-records')
            call list_add(args, '32')
            call gremlin_json(driver, project, cache, state, args, status, result, 30000)
            call assert_true(result%exit_code == 0, &
                'reads the public Gremlin receipt stream')
            events = json_member(status, 'events')
            do i = 1, json_size(events)
                event = json_element(events, i)
                if (field(event, 'generation') /= generation_id) cycle
                if (field(event, 'case_id') /= 'test_provenance') cycle
                if (field(event, 'status') /= 'PASS') cycle
                if (present(completion_id)) then
                    if (field(event, 'completion_id') /= completion_id) cycle
                end if
                receipt = event
                return
            end do
            cursor_value = json_member(status, 'next_cursor')
            more_value = json_member(status, 'has_more')
            next_cursor = int(json_number_value(cursor_value))
            has_more = json_boolean_value(more_value)
            if (.not. has_more) return
            if (next_cursor <= cursor) then
                call assert_true(.false., 'receipt pagination cursor advances')
                return
            end if
            cursor = next_cursor
        end do
        call assert_true(.false., 'receipt search stays within bounded pagination')
    end subroutine find_pass_receipt

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
