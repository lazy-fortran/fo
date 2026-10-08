program test_gremlin_watch
    use, intrinsic :: iso_c_binding, only: c_char, c_int, c_null_char
    use fo_test_harness, only: string_list_t, process_result_t, list_add
    use fo_test_harness, only: make_directory, write_text, read_text, run_process
    use fo_test_harness, only: file_exists
    use fo_test_harness, only: move_path
    use fo_test_harness, only: assert_true, assert_equal_integer, assert_equal_string
    use fo_test_harness, only: finish_assertions
    use fo_test_gremlin_oracle, only: gremlin_setup, gremlin_json, gremlin_run
    use fo_test_gremlin_oracle, only: gremlin_start_args, gremlin_wait_ms, gremlin_wait_file
    use fo_test_gremlin_oracle, only: gremlin_fifo
    use fo_test_gremlin_oracle, only: gremlin_spawn, gremlin_stop_child
    use fo_test_json, only: json_value_t, json_member
    use fo_test_json, only: json_string_value, json_number_value, json_parse
    use fo_test_gremlin_oracle, only: gremlin_stop_lane
    implicit none

    interface
        integer(c_int) function c_setenv(name, value, overwrite) bind(C, name='setenv')
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: name(*), value(*)
            integer(c_int), value :: overwrite
        end function c_setenv
    end interface

    character(:), allocatable :: driver, scratch, project, cache, state, counter
    character(:), allocatable :: dependency, nested, lane, session, generation
    character(:), allocatable :: gate, entered, manifest, retired_path, failed_counter
    character(:), allocatable :: cli_project, bin_dir, path_value, check_marker
    character(:), allocatable :: temp_one, temp_two, temp_populated
    type(string_list_t) :: args, bad_observer
    type(process_result_t) :: process
    type(json_value_t) :: response, status
    integer :: before, attempt, position, watch_pid, watch_exit, path_length, env_result
    logical :: found

    call gremlin_setup(driver, scratch, project, cache, state, counter)
    dependency = scratch//'/dependency'
    nested = project//'/build/nested_dependency'
    lane = 'watch-event-oracle'
    gate = scratch//'/watch-test.fifo'
    entered = scratch//'/watch-test.started'
    retired_path = scratch//'/dependency-retired'
    call make_directory(project//'/src')
    call make_directory(project//'/test')
    call make_directory(dependency//'/src')
    call make_directory(nested//'/src')
    manifest = 'name = "gremlin_watch_probe"'//new_line('a')// &
        '[dependencies]'//new_line('a')// &
        'watch_dep = { path = "../dependency" }'//new_line('a')// &
        'nested_dep = { path = "build/nested_dependency" }'//new_line('a')
    call write_text(project//'/fpm.toml', manifest)
    call write_text(dependency//'/fpm.toml', 'name = "watch_dep"'//new_line('a'))
    call write_text(nested//'/fpm.toml', 'name = "nested_dep"'//new_line('a'))
    call write_text(dependency//'/src/depmod.f90', &
        'module depmod'//new_line('a')//'end module depmod'//new_line('a'))
    call write_text(nested//'/src/nested.f90', &
        'module nested_dep'//new_line('a')//'end module nested_dep'//new_line('a'))
    call write_text(project//'/src/oracle.data', 'captured input'//new_line('a'))
    call gremlin_fifo(gate)
    call write_blocked_test()
    call gremlin_start_args(args, project, lane, 'test_watch')
    call gremlin_json(driver, project, cache, state, args, response, process, 120000)
    session = field(response, 'session_id')
    call assert_true(len(session) > 0, 'watch session starts')
    call gremlin_wait_file(entered, 120000, found)
    call assert_true(found, 'real test process reaches the FIFO barrier')
    call wait_status(status)
    generation = field(status, 'active_generation')
    before = capture_count()
    call gremlin_wait_ms(1200)
    call assert_equal_integer(capture_count(), before, 'idle watch does not recapture')
    call assert_equal_string(field_status(session, 'active_generation'), generation, &
        'idle watch retains its generation')

    call expect_one_capture(project//'/test/test_watch.f90', &
        read_text(project//'/test/test_watch.f90')//new_line('a')//'! source edit'//new_line('a'))
    call expect_one_capture(project//'/test/test_watch.f90', &
        read_text(project//'/test/test_watch.f90')//'! burst a'//new_line('a')// &
        '! burst b'//new_line('a')//'! burst c'//new_line('a'))
    before = capture_count()
    call move_file(project//'/src/oracle.data', project//'/src/oracle.renamed')
    call wait_generation_after(before, generation)
    call assert_equal_integer(capture_count(), before + 1, 'rename is one event burst')
    before = capture_count()
    call remove_file(project//'/src/oracle.renamed')
    call wait_generation_after(before, generation)
    call assert_equal_integer(capture_count(), before + 1, 'unlink is one event burst')
    call expect_one_capture(project//'/fpm.toml', read_text(project//'/fpm.toml')// &
        '# manifest edit'//new_line('a'))
    call expect_one_capture(dependency//'/src/depmod.f90', &
        read_text(dependency//'/src/depmod.f90')//'! dependency edit'//new_line('a'))
    call expect_one_capture(nested//'/src/nested.f90', &
        read_text(nested//'/src/nested.f90')//'! nested dependency edit'//new_line('a'))
    before = capture_count()
    call move_path(dependency, retired_path)
    call make_directory(dependency//'/src')
    call write_text(dependency//'/fpm.toml', 'name = "watch_dep"'//new_line('a'))
    call write_text(dependency//'/src/depmod.f90', &
        'module depmod'//new_line('a')//'implicit none'//new_line('a')// &
        'integer, parameter :: dep_value = 1'//new_line('a')// &
        'end module depmod'//new_line('a'))
    call wait_generation_after(before, generation)
    call assert_equal_integer(capture_count(), before + 1, &
        'dependency root replacement recovers with one immutable capture')
    ! Keep nested creation immediately after root replacement to exercise recovery.
    call make_directory(project//'/src/new/deep')
    call expect_one_capture(project//'/src/new/deep/input.data', 'new nested input'//new_line('a'))
    call expect_one_capture(project//'/src/new/deep/input.data', &
        read_text(project//'/src/new/deep/input.data')//'updated input'//new_line('a'))

    manifest = read_text(project//'/fpm.toml')
    position = index(manifest, 'nested_dep =')
    if (position > 0) manifest = manifest(:position - 1)
    call expect_one_capture(project//'/fpm.toml', manifest)
    before = capture_count()
    call write_text(nested//'/src/nested.f90', 'module nested_dep'//new_line('a')// &
        '! retired root edit'//new_line('a')//'end module nested_dep'//new_line('a'))
    call gremlin_wait_ms(900)
    call assert_equal_integer(capture_count(), before, 'retired dependency roots are unsubscribed')
    call make_directory(project//'/.git')
    call write_text(project//'/.git/ignored', 'metadata'//new_line('a'))
    call make_directory(project//'/.bzr')
    call write_text(project//'/.bzr/ignored', 'metadata'//new_line('a'))
    call write_text(scratch//'/outside.txt', 'outside closure'//new_line('a'))
    call gremlin_wait_ms(900)
    call assert_equal_integer(capture_count(), before, 'metadata and outside edits are ignored')
    call stop_lane(session)

    failed_counter = scratch//'/missing-observer-parent/counter.json'
    call gremlin_start_args(args, project, 'watch-observer-error', 'test_watch')
    call list_add(bad_observer, 'FO_GREMLIN_TEST_CAPTURE_COUNTER='//failed_counter)
    call gremlin_run(driver, project, cache, state, args, process, bad_observer, 120000)
    call assert_equal_integer(process%exit_code, 0, 'observer-error lane still starts')
    call json_parse(process%stdout, response, found, manifest)
    call assert_true(found, 'observer-error start returns JSON')
    session = field(response, 'session_id')
    call assert_true(len(session) > 0, 'observer-error lane returns an owner')
    call wait_status_for('watch-observer-error', session, status)
    call assert_true(len(field(status, 'active_generation')) > 0, &
        'capture succeeds when its optional observer cannot write')
    call assert_true(.not. file_exists(failed_counter), 'observer failure was induced')
    call stop_observer_lane(session)

    cli_project = scratch//'/watch-cli-project'
    bin_dir = scratch//'/watch-cli-bin'
    call make_directory(cli_project//'/src')
    call make_directory(bin_dir)
    call write_text(cli_project//'/fpm.toml', 'name = "watch_cli_probe"'//new_line('a'))
    call write_text(cli_project//'/src/sample.f90', &
        'module sample'//new_line('a')//'integer :: sample_value'//new_line('a')// &
        'end module sample'//new_line('a'))
    call write_text(cli_project//'/src/second.f90', &
        'module second'//new_line('a')//'end module second'//new_line('a'))
    check_marker = scratch//'/watch-checks'
    call write_text(scratch//'/check-command.c', &
        '#include <stdio.h>'//new_line('a')// &
        'int main(void){FILE *f=fopen("'//check_marker//'","a");'//new_line('a')// &
        'if(!f)return 1;fputs("check",f);fputc(10,f);return fclose(f)!=0;}'//new_line('a'))
    args = string_list_t()
    call list_add(args, 'cc')
    call list_add(args, '-o')
    call list_add(args, bin_dir//'/fo')
    call list_add(args, scratch//'/check-command.c')
    call run_process(args, scratch, process, timeout_ms=30000)
    call assert_equal_integer(process%exit_code, 0, 'builds the independent check recorder')
    call get_environment_variable('PATH', length=path_length)
    allocate(character(len=path_length) :: path_value)
    call get_environment_variable('PATH', path_value)
    env_result = c_setenv('PATH'//c_null_char, &
        (bin_dir//':'//trim(path_value))//c_null_char, 1_c_int)
    call assert_true(env_result == 0, 'watch fixture prepends isolated check command')
    args = string_list_t()
    call list_add(args, 'watch')
    call list_add(args, '--fmt')
    call gremlin_spawn(driver, cli_project, args, scratch//'/watch-cli.out', &
        scratch//'/watch-cli.err', watch_pid)
    call gremlin_wait_ms(300)
    call write_text(cli_project//'/src/sample.f90', &
        'module sample'//new_line('a')//'integer :: sample_value'//new_line('a')// &
        'end module sample'//new_line('a')//'! first edit'//new_line('a'))
    call wait_for_format(cli_project//'/src/sample.f90', 'sample_value')
    call wait_for_checks(1)
    temp_one = scratch//'/sample.atomic'
    temp_two = scratch//'/second.atomic'
    call write_text(temp_one, 'module sample'//new_line('a')// &
        'integer :: sample_value'//new_line('a')//'end module sample'//new_line('a'))
    call write_text(temp_two, 'module second'//new_line('a')// &
        'integer :: second_value'//new_line('a')//'end module second'//new_line('a'))
    call move_path(temp_one, cli_project//'/src/sample.f90')
    call move_path(temp_two, cli_project//'/src/second.f90')
    call wait_for_format(cli_project//'/src/sample.f90', 'sample_value')
    call wait_for_format(cli_project//'/src/second.f90', 'second_value')
    call wait_for_checks(2)
    call make_directory(cli_project//'/src/created/deep')
    temp_populated = scratch//'/populated.atomic'
    call write_text(temp_populated, 'module populated'//new_line('a')// &
        'integer :: populated_value'//new_line('a')//'end module populated'//new_line('a'))
    call move_path(temp_populated, cli_project//'/src/created/deep/populated.f90')
    call wait_for_format(cli_project//'/src/created/deep/populated.f90', 'populated_value')
    call wait_for_checks(3)
    call gremlin_wait_ms(700)
    call assert_equal_integer(check_count(), 3, 'formatting writes do not cause a check loop')
    call gremlin_stop_child(watch_pid, watch_exit)
    call finish_assertions()

contains

    integer function check_count() result(count)
        character(:), allocatable :: text
        integer :: i
        count = 0
        if (.not. file_exists(check_marker)) return
        text = read_text(check_marker)
        do i = 1, len(text)
            if (text(i:i) == new_line('a')) count = count + 1
        end do
    end function check_count

    subroutine wait_for_checks(expected)
        integer, intent(in) :: expected
        integer :: attempt
        do attempt = 1, 100
            if (check_count() >= expected) exit
            call gremlin_wait_ms(50)
        end do
        call assert_equal_integer(check_count(), expected, &
            'one debounced check follows each complete external edit burst')
    end subroutine wait_for_checks

    function field(object, key) result(value)
        type(json_value_t), intent(in) :: object
        character(len=*), intent(in) :: key
        character(:), allocatable :: value
        type(json_value_t) :: item
        item = json_member(object, key)
        value = json_string_value(item)
    end function field

    integer function capture_count() result(count)
        type(json_value_t) :: object, item
        logical :: valid
        character(:), allocatable :: message
        call json_parse(read_text(counter), object, valid, message)
        call assert_true(valid, 'capture counter remains valid JSON: '//message)
        item = json_member(object, 'count')
        count = int(json_number_value(item))
    end function capture_count

    subroutine wait_generation_after(baseline, old_generation)
        integer, intent(in) :: baseline
        character(:), allocatable, intent(inout) :: old_generation
        type(json_value_t) :: current
        integer :: attempt
        do attempt = 1, 300
            call status_for(session, current)
            if (field(current, 'active_generation') /= old_generation .and. &
                capture_count() > baseline) then
                old_generation = field(current, 'active_generation')
                call gremlin_wait_ms(700)
                return
            end if
            call gremlin_wait_ms(50)
        end do
        call assert_true(.false., 'filesystem event publishes a new generation')
    end subroutine wait_generation_after

    subroutine move_file(source, destination)
        character(len=*), intent(in) :: source, destination
        type(string_list_t) :: command
        type(process_result_t) :: result
        call list_add(command, 'mv')
        call list_add(command, source)
        call list_add(command, destination)
        call run_process(command, scratch, result, timeout_ms=10000)
        call assert_equal_integer(result%exit_code, 0, 'rename fixture file succeeds')
    end subroutine move_file

    subroutine remove_file(path)
        character(len=*), intent(in) :: path
        type(string_list_t) :: command
        type(process_result_t) :: result
        call list_add(command, 'rm')
        call list_add(command, path)
        call run_process(command, scratch, result, timeout_ms=10000)
        call assert_equal_integer(result%exit_code, 0, 'unlink fixture file succeeds')
    end subroutine remove_file

    subroutine write_blocked_test()
        character(:), allocatable :: source
        source = 'program test_watch'//new_line('a')//'implicit none'//new_line('a')// &
            'integer :: unit'//new_line('a')//'character :: token'//new_line('a')// &
            "open(newunit=unit,file='"//entered//"',status='replace')"//new_line('a')// &
            "write(unit,'(a)') 'started'"//new_line('a')//'close(unit)'//new_line('a')// &
            "open(newunit=unit,file='"//gate//"',status='old',access='stream', &"//new_line('a')// &
            " form='unformatted',action='read')"//new_line('a')//'read(unit) token'//new_line('a')// &
            'close(unit)'//new_line('a')//'end program test_watch'//new_line('a')
        call write_text(project//'/test/test_watch.f90', source)
    end subroutine write_blocked_test

    subroutine status_for(owner, document)
        character(len=*), intent(in) :: owner
        type(json_value_t), intent(out) :: document
        type(string_list_t) :: values
        type(process_result_t) :: result
        call list_add(values, 'gremlin')
        call list_add(values, 'status')
        call list_add(values, '--detail')
        call list_add(values, 'full')
        call list_add(values, '--dir')
        call list_add(values, project)
        call list_add(values, '--lane')
        call list_add(values, lane)
        call list_add(values, '--session')
        call list_add(values, owner)
        call list_add(values, '--cursor')
        call list_add(values, '0')
        call gremlin_json(driver, project, cache, state, values, document, result, 30000)
    end subroutine status_for

    subroutine wait_status_for(lane_id, owner, document)
        character(len=*), intent(in) :: lane_id, owner
        type(json_value_t), intent(out) :: document
        type(string_list_t) :: values
        type(process_result_t) :: result
        integer :: attempt
        do attempt = 1, 300
            call list_add(values, 'gremlin')
            call list_add(values, 'status')
            call list_add(values, '--detail')
            call list_add(values, 'full')
            call list_add(values, '--dir')
            call list_add(values, project)
            call list_add(values, '--lane')
            call list_add(values, lane_id)
            call list_add(values, '--session')
            call list_add(values, owner)
            call gremlin_json(driver, project, cache, state, values, document, result, 30000)
            if (len(field(document, 'active_generation')) > 0) return
            call gremlin_wait_ms(100)
            values = string_list_t()
        end do
        call assert_true(.false., 'observer-error session publishes a generation')
    end subroutine wait_status_for

    subroutine stop_observer_lane(owner)
        character(len=*), intent(in) :: owner
        call gremlin_stop_lane(driver, project, cache, state, "watch-observer-error", owner)
    end subroutine stop_observer_lane

    subroutine wait_for_format(path, variable)
        character(len=*), intent(in) :: path, variable
        character(:), allocatable :: content
        integer :: attempt
        do attempt = 1, 200
            content = read_text(path)
            if (index(content, '    integer :: '//variable) > 0) return
            call gremlin_wait_ms(50)
        end do
        call assert_true(.false., 'fo watch --fmt formats '//path)
    end subroutine wait_for_format

    subroutine wait_status(document)
        type(json_value_t), intent(out) :: document
        integer :: attempt
        do attempt = 1, 300
            call status_for(session, document)
            if (len(field(document, 'active_generation')) > 0) return
            call gremlin_wait_ms(100)
        end do
        call assert_true(.false., 'watch publishes an initial generation')
    end subroutine wait_status

    function field_status(owner, key) result(value)
        character(len=*), intent(in) :: owner, key
        character(:), allocatable :: value
        type(json_value_t) :: document
        call status_for(owner, document)
        value = field(document, key)
    end function field_status

    subroutine expect_one_capture(path, content)
        character(len=*), intent(in) :: path, content
        type(json_value_t) :: current
        character(:), allocatable :: old_generation
        integer :: baseline, attempt
        baseline = capture_count()
        call status_for(session, current)
        old_generation = field(current, 'active_generation')
        call write_text(path, content)
        do attempt = 1, 300
            call status_for(session, current)
            if (field(current, 'active_generation') /= old_generation .and. &
                capture_count() > baseline) exit
            call gremlin_wait_ms(50)
        end do
        call assert_true(field(current, 'active_generation') /= old_generation, &
            'relevant edit publishes a generation: '//path)
        call gremlin_wait_ms(700)
        call assert_equal_integer(capture_count(), baseline + 1, &
            'one edit burst makes exactly one immutable capture: '//path)
    end subroutine expect_one_capture

    subroutine stop_lane(owner)
        character(len=*), intent(in) :: owner
        call gremlin_stop_lane(driver, project, cache, state, lane, owner)
    end subroutine stop_lane

end program test_gremlin_watch
