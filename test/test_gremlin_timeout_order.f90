program test_gremlin_timeout_order
    use, intrinsic :: iso_c_binding, only: c_char, c_int, c_null_char, c_null_ptr
    use fo_test_harness, only: string_list_t, process_result_t, list_add
    use fo_test_harness, only: make_directory, write_text, file_exists, process_alive
    use fo_test_harness, only: remove_path
    use fo_test_harness, only: run_process, list_with_first, assert_true
    use fo_test_harness, only: assert_equal_string, assert_equal_integer, finish_assertions
    use fo_test_gremlin_oracle, only: gremlin_setup, gremlin_run, gremlin_json
    use fo_test_gremlin_oracle, only: gremlin_wait_file, gremlin_wait_ms
    use fo_test_gremlin_oracle, only: gremlin_fifo, gremlin_release_fifo
    use fo_test_json, only: json_value_t, json_member, json_element, json_size
    use fo_test_json, only: json_string_value, json_number_value, json_parse
    use fo_test_gremlin_oracle, only: gremlin_stop_lane
    implicit none

    character(:), allocatable :: driver, scratch, project, cache, state, lane
    character(:), allocatable :: started, pid_path, done, gate, hold, release, entered
    character(:), allocatable :: compiler, compiler_dir, compiler_source, path_value
    character(:), allocatable :: session, generation
    type(string_list_t) :: args, extra, command
    type(process_result_t) :: process
    type(json_value_t) :: report, event
    integer :: path_length, path_status, test_pid
    logical :: found, valid
    character(:), allocatable :: message

    interface
        integer(c_int) function c_utime(path, times) bind(C, name='utime')
            use, intrinsic :: iso_c_binding, only: c_char, c_int, c_ptr
            character(kind=c_char), intent(in) :: path(*)
            type(c_ptr), value :: times
        end function c_utime
    end interface

    call gremlin_setup(driver, scratch, project, cache, state)
    call get_environment_variable('PATH', length=path_length, status=path_status)
    call assert_true(path_status == 0 .and. path_length > 0, 'fixture has a process PATH')
    allocate(character(len=path_length) :: path_value)
    call get_environment_variable('PATH', path_value)
    lane = 'timeout-order'
    started = scratch//'/test.started'
    pid_path = scratch//'/test.pid'
    done = scratch//'/test.done'
    gate = scratch//'/test.gate'
    hold = scratch//'/hold-version'
    release = scratch//'/release-version'
    entered = scratch//'/version-entered'
    call make_directory(project//'/test')
    call write_text(project//'/fpm.toml', &
        'name = "gremlin_timeout_order_probe"'//new_line('a'))
    call gremlin_fifo(gate)
    call write_case()
    call locate_compiler(compiler)
    compiler_dir = scratch//'/compiler-bin'
    call make_directory(compiler_dir)
    compiler_source = scratch//'/compiler-wrapper.c'
    call write_wrapper(compiler_source, compiler, hold, release, entered)
    call list_add(args, 'cc')
    call list_add(args, '-o')
    call list_add(args, compiler_dir//'/gfortran')
    call list_add(args, compiler_source)
    call run_process(args, scratch, process, timeout_ms=30000)
    call assert_true(process%exit_code == 0, 'compiles the real compiler barrier wrapper')

    call start_lane(lane, .true., report)
    session = field(report, 'session_id')
    call wait_for_file(started, 120000, found)
    call assert_true(found, 'captured test reaches its FIFO')
    call status_now(lane, session, report)
    generation = field(report, 'active_generation')
    call wait_for_file(pid_path, 5000, found)
    call assert_true(found, 'blocked test publishes a complete PID record')
    test_pid = file_integer(pid_path)
    call assert_true(test_pid > 0, 'blocked test publishes its PID')
    call write_text(hold, 'hold'//new_line('a'))
    call touch_directory(project)
    call wait_for_file(entered, 5000, found)
    call assert_true(found, 'new context capture is held in the compiler version probe')

    ! The test exits before its budget while generation capture is still blocked.
    ! Gremlin must retain its timer observation until capture returns, then publish PASS.
    call gremlin_release_fifo(gate)
    call wait_for_file(done, 5000, found)
    call assert_true(found, 'short child completes while compiler probe is stalled')
    call gremlin_wait_ms(5400)
    call status_now(lane, session, report)
    call assert_true(.not. has_case(report, 'test_timeout_order', generation), &
        'capture stall does not publish a premature timeout')
    call assert_true(.not. process_alive(test_pid), 'completed test child has exited')
    call write_text(release, 'release'//new_line('a'))
    call wait_for_case(lane, session, 'PASS', event)
    generation = field(event, 'generation')
    call assert_equal_string(field(event, 'status'), 'PASS', &
        'completed child is classified after the capture probe returns')
    call assert_true(len(generation) == 64, 'receipt retains its immutable generation')
    call stop_lane(lane, session)

    ! Control: a child which stays blocked beyond its budget receives TIMEOUT/124
    ! and is terminated while the same compiler wrapper now answers immediately.
    call remove_file(started)
    call remove_file(pid_path)
    call remove_file(done)
    call remove_path(gate)
    call gremlin_fifo(gate)
    lane = 'timeout-order-control'
    call start_lane(lane, .false., report)
    session = field(report, 'session_id')
    call wait_for_file(started, 120000, found)
    call assert_true(found, 'timeout control child reaches its FIFO')
    call wait_for_file(pid_path, 5000, found)
    call assert_true(found, 'timeout control publishes a complete PID record')
    test_pid = file_integer(pid_path)
    call assert_true(test_pid > 0, 'timeout child publishes its PID')
    call wait_for_case(lane, session, 'TIMEOUT', event, 15000)
    call assert_equal_integer(int(json_number_value(json_member(event, 'exitcode'))), 124, &
        'genuine deadline expiry uses exit code 124')
    call assert_true(.not. file_exists(done), 'timed-out child never reaches completion marker')
    call gremlin_wait_ms(100)
    call assert_true(.not. process_alive(test_pid), 'timed-out test child is terminated')
    call stop_lane(lane, session)
    call finish_assertions()

contains

    function field(document, key) result(value)
        type(json_value_t), intent(in) :: document
        character(len=*), intent(in) :: key
        character(:), allocatable :: value
        type(json_value_t) :: item
        item = json_member(document, key)
        value = json_string_value(item)
    end function field

    subroutine write_case()
        character(:), allocatable :: source
        source = 'program test_timeout_order'//new_line('a')// &
            'use, intrinsic :: iso_c_binding, only: c_int'//new_line('a')//'implicit none'//new_line('a')// &
            'interface'//new_line('a')// &
            '    function c_getpid() bind(C, name="getpid") result(pid)'//new_line('a')// &
            '        import :: c_int'//new_line('a')//'        integer(c_int) :: pid'//new_line('a')// &
            '    end function c_getpid'//new_line('a')//'end interface'//new_line('a')// &
            'integer :: unit, gate_unit'//new_line('a')//'character :: token'//new_line('a')// &
            "open(newunit=unit,file='"//started//"',status='replace')"//new_line('a')// &
            "write(unit,'(a)') 'started'"//new_line('a')//'close(unit)'//new_line('a')// &
            "open(newunit=unit,file='"//pid_path//"',status='replace')"//new_line('a')// &
            "write(unit,'(i0)') c_getpid()"//new_line('a')//'close(unit)'//new_line('a')// &
            "open(newunit=gate_unit,file='"//gate//"',status='old',access='stream', &"//new_line('a')// &
            "    form='unformatted',action='read')"//new_line('a')// &
            'read(gate_unit) token'//new_line('a')//'close(gate_unit)'//new_line('a')// &
            "open(newunit=unit,file='"//done//"',status='replace')"//new_line('a')// &
            "write(unit,'(a)') 'done'"//new_line('a')//'close(unit)'//new_line('a')// &
            'end program test_timeout_order'//new_line('a')
        call write_text(project//'/test/test_timeout_order.f90', source)
    end subroutine write_case

    subroutine write_wrapper(path, real_compiler, hold_file, release_file, entered_file)
        character(len=*), intent(in) :: path, real_compiler, hold_file, release_file, entered_file
        character(:), allocatable :: source
        source = '#include <stdio.h>'//new_line('a')// &
            '#include <stdlib.h>'//new_line('a')//'#include <string.h>'//new_line('a')// &
            '#include <unistd.h>'//new_line('a')//'#include <time.h>'//new_line('a')// &
            'int main(int argc,char **argv){'//new_line('a')// &
            'const char *hold=getenv("FO_TIMEOUT_VERSION_HOLD");'//new_line('a')// &
            'if(argc==2 && strcmp(argv[1],"--version")==0 && hold && access(hold,F_OK)==0){'//new_line('a')// &
            'const char *p=getenv("FO_TIMEOUT_VERSION_ENTERED"); FILE *f=fopen(p,"w");'//new_line('a')// &
            'if(!f)return 120; fprintf(f,"%ld'//achar(92)// &
            'n",(long)getpid()); fclose(f);'//new_line('a')// &
            'const char *r=getenv("FO_TIMEOUT_VERSION_RELEASE"); struct timespec t={0,20000000};'//new_line('a')// &
            'while(access(r,F_OK)!=0)nanosleep(&t,0); }'//new_line('a')// &
            'argv[0]="'//real_compiler//'";'//new_line('a')// &
            'execv(argv[0],argv); return 127; }'//new_line('a')
        call write_text(path, source)
    end subroutine write_wrapper

    subroutine locate_compiler(path)
        character(:), allocatable, intent(out) :: path
        type(string_list_t) :: args
        type(process_result_t) :: result
        call list_add(args, 'gfortran')
        call list_with_first('which', args, command)
        call run_process(command, project, result, timeout_ms=10000)
        call assert_equal_integer(result%exit_code, 0, 'locates the real compiler')
        path = result%stdout(:index(result%stdout, new_line('a')) - 1)
    end subroutine locate_compiler

    subroutine start_lane(lane_id, block_probe, value)
        character(len=*), intent(in) :: lane_id
        logical, intent(in) :: block_probe
        type(json_value_t), intent(out) :: value
        type(string_list_t) :: values, environment
        type(process_result_t) :: result
        logical :: ok
        call list_add(values, 'gremlin')
        call list_add(values, 'start')
        call list_add(values, '--dir')
        call list_add(values, project)
        call list_add(values, '--lane')
        call list_add(values, lane_id)
        call list_add(values, '--target')
        call list_add(values, 'test_timeout_order')
        call list_add(values, '--campaign-seconds')
        call list_add(values, '60')
        call list_add(values, '--seed')
        call list_add(values, '1729')
        call list_add(values, '--timeout-seconds')
        call list_add(values, '5')
        call list_add(environment, 'PATH='//compiler_dir//':'//path_value)
        if (block_probe) then
            call list_add(environment, 'FO_TIMEOUT_VERSION_HOLD='//hold)
            call list_add(environment, 'FO_TIMEOUT_VERSION_RELEASE='//release)
            call list_add(environment, 'FO_TIMEOUT_VERSION_ENTERED='//entered)
        end if
        call gremlin_run(driver, project, cache, state, values, result, environment, 30000)
        call assert_true(result%exit_code == 0, 'starts timeout-order lane '//lane_id)
        call json_parse(result%stdout, value, ok, message)
        call assert_true(ok, 'start returns JSON: '//message)
    end subroutine start_lane

    subroutine status_now(lane_id, owner, value)
        character(len=*), intent(in) :: lane_id, owner
        type(json_value_t), intent(out) :: value
        type(string_list_t) :: values
        type(process_result_t) :: result
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
        call list_add(values, '--cursor')
        call list_add(values, '0')
        call gremlin_json(driver, project, cache, state, values, value, result, 30000)
    end subroutine status_now

    logical function has_case(status, case_id, generation)
        type(json_value_t), intent(in) :: status
        character(len=*), intent(in) :: case_id, generation
        type(json_value_t) :: events, item
        integer :: j
        has_case = .false.
        events = json_member(status, 'events')
        do j = 1, json_size(events)
            item = json_element(events, j)
            if (field(item, 'case_id') /= case_id) cycle
            if (len(generation) > 0) then
                if (field(item, 'generation') /= generation) cycle
            end if
            has_case = .true.
            return
        end do
    end function has_case

    subroutine wait_for_case(lane_id, owner, expected, found_event, timeout_ms)
        character(len=*), intent(in) :: lane_id, owner, expected
        type(json_value_t), intent(out) :: found_event
        integer, optional, intent(in) :: timeout_ms
        type(json_value_t) :: status, events, item
        integer :: attempt, limit, j
        limit = 300
        if (present(timeout_ms)) limit = max(1, timeout_ms / 100)
        do attempt = 1, limit
            call status_now(lane_id, owner, status)
            events = json_member(status, 'events')
            do j = 1, json_size(events)
                item = json_element(events, j)
                if (field(item, 'case_id') /= 'test_timeout_order') cycle
                if (field(item, 'status') == expected) then
                    found_event = item
                    return
                end if
            end do
            call gremlin_wait_ms(100)
        end do
        call assert_true(.false., 'waits for timeout-order '//expected//' event')
    end subroutine wait_for_case

    subroutine wait_for_file(path, timeout_ms, exists)
        character(len=*), intent(in) :: path
        integer, intent(in) :: timeout_ms
        logical, intent(out) :: exists
        call gremlin_wait_file(path, timeout_ms, exists)
    end subroutine wait_for_file

    integer function file_integer(path)
        character(len=*), intent(in) :: path
        character(len=64) :: text
        integer :: unit, ios
        file_integer = -1
        open(newunit=unit, file=path, status='old', action='read', iostat=ios)
        if (ios /= 0) return
        read(unit, *, iostat=ios) file_integer
        close(unit)
        if (ios /= 0) file_integer = -1
    end function file_integer

    subroutine touch_directory(path)
        character(len=*), intent(in) :: path
        integer(c_int) :: rc
        rc = c_utime(trim(path)//c_null_char, c_null_ptr)
        call assert_true(rc == 0, 'touches project directory to produce a real capture event')
    end subroutine touch_directory

    subroutine remove_file(path)
        character(len=*), intent(in) :: path
        integer :: unit, ios
        if (.not. file_exists(path)) return
        open(newunit=unit, file=path, status='old', iostat=ios)
        if (ios == 0) close(unit, status='delete', iostat=ios)
        call assert_true(ios == 0, 'removes prior test marker')
    end subroutine remove_file

    subroutine stop_lane(lane_id, owner)
        character(len=*), intent(in) :: lane_id, owner
        call gremlin_stop_lane(driver, project, cache, state, lane_id, owner)
    end subroutine stop_lane

end program test_gremlin_timeout_order
