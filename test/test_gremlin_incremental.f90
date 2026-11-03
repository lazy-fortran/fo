program test_gremlin_incremental
    !! Observe real Make/Ninja objects across generations and a failed candidate.
    use, intrinsic :: iso_c_binding, only: c_char, c_int, c_long_long, c_null_char
    use fo_test_harness, only: string_list_t, process_result_t, list_add, &
        write_text, read_text, assert_true, assert_contains, finish_assertions
    use fo_test_gremlin_oracle, only: gremlin_setup, gremlin_start_args, &
        gremlin_json, gremlin_field, gremlin_wait_ms, gremlin_stop_lane
    use fo_test_cli, only: run_external
    use fo_test_json, only: json_value_t, json_member, json_boolean_value
    use fo_fs, only: fs_stat, fs_find_executable
    implicit none
    character(:), allocatable :: driver, scratch, project, cache, state, session
    character(:), allocatable :: original, roots, root_a, root_b, object, lane
    character(len=4096) :: executable
    character, parameter :: nl = new_line('a')
    type(string_list_t) :: args
    type(process_result_t) :: process
    type(json_value_t) :: reply
    integer(c_long_long) :: before_a, before_b, after, bytes
    integer :: rc, generator, at, ending, attempt, configurations
    logical :: found, ok
    interface
        integer(c_int) function set_env(name, value, overwrite) bind(C, name='setenv')
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: name(*), value(*)
            integer(c_int), value :: overwrite
        end function set_env
    end interface

    call fs_find_executable('ninja', executable, found)
    if (.not. found) stop 'SKIP: Ninja is unavailable'
    do generator = 1, 2
        call gremlin_setup(driver, scratch, project, cache, state)
        lane = 'incremental'
        call write_text(project//'/CMakeLists.txt', &
            'cmake_minimum_required(VERSION 3.22)'//nl// &
            'project(incremental LANGUAGES Fortran)'//nl// &
            'enable_testing()'//nl// &
            'file(APPEND "'//scratch//'/roots" "${CMAKE_SOURCE_DIR}\n")'//nl// &
            'add_library(core stable.f90 value.f90)'//nl// &
            'add_executable(probe probe.f90)'//nl// &
            'target_link_libraries(probe PRIVATE core)'//nl// &
            'add_test(NAME incremental_case COMMAND probe)'//nl)
        call write_text(project//'/stable.f90', &
            'module stable_mod'//nl//'contains'//nl// &
            'integer function stable()'//nl//'stable=7'//nl// &
            'end function'//nl//'end module'//nl)
        call value_source(1)
        call write_text(project//'/probe.f90', &
            'program probe'//nl//'use stable_mod'//nl//'use value_mod'//nl// &
            'open(10,file="'//scratch//'/observed",status="replace")'//nl// &
            'write(10,"(i0)") stable()+value()'//nl//'close(10)'//nl// &
            'end program'//nl)
        ! Exercise each generator against a separately configured caller tree.
        args = string_list_t()
        call list_add(args, '-S')
        call list_add(args, project)
        call list_add(args, '-B')
        call list_add(args, project//'/build')
        call list_add(args, '-G')
        if (generator == 1) then
            call list_add(args, 'Ninja')
        else
            call list_add(args, 'Unix Makefiles')
        end if
        call run_external('cmake', args, project, process, timeout_ms=30000)
        call assert_true(process%exit_code == 0, 'creates the caller generator tree')
        rc = set_env('FO_CMAKE_GENERATOR'//c_null_char, 'Ninja'//c_null_char, 1_c_int)
        if (generator == 2) rc = set_env('FO_CMAKE_GENERATOR'//c_null_char, &
            'Unix Makefiles'//c_null_char, 1_c_int)
        rc = set_env('FO_CMAKE_DELEGATE_BUILD'//c_null_char, '1'//c_null_char, 1_c_int)
        call gremlin_start_args(args, project, lane, 'incremental_case')
        call list_add(args, '--random')
        call list_add(args, '0')
        call gremlin_json(driver, project, cache, state, args, reply, process, 20000)
        session = gremlin_field(reply, 'session_id')
        call assert_true(len(session) > 0, 'starts incremental fixture')
        if (len(session) == 0) exit
        call wait_green('')
        original = gremlin_field(reply, 'active_generation')
        call assert_contains(read_text(scratch//'/observed'), '8', 'runs original code')
        roots = read_text(scratch//'/roots')
        root_a = ''
        root_b = ''
        at = 1
        do while (at <= len(roots))
            ending = index(roots(at:), nl)
            if (ending == 0) exit
            object = roots(at:at + ending - 2)
            if (index(object, '/views/execution-') > 0) then
                if (len(root_a) == 0) root_a = object
                if (object /= root_a) root_b = object
            end if
            at = at + ending
        end do
        call assert_true(len(root_a) > 0 .and. len(root_b) > 0, 'warms two fixed slots')
        if (len(root_a) > 0 .and. len(root_b) > 0) then
            object = '/build/CMakeFiles/core.dir/stable.f90.o'
            call fs_stat(root_a//object, before_a, bytes, ok)
            call assert_true(ok, 'first slot contains the unedited object')
            call fs_stat(root_b//object, before_b, bytes, ok)
            call assert_true(ok, 'second slot contains the unedited object')
            if (generator == 1) then
                call assert_contains(read_text(root_a//'/build/CMakeCache.txt'), &
                    'CMAKE_GENERATOR:INTERNAL=Ninja', 'explicit Ninja selection is preserved')
            else
                call assert_contains(read_text(root_a//'/build/CMakeCache.txt'), &
                    'CMAKE_GENERATOR:INTERNAL=Unix Makefiles', &
                    'an explicitly selected generator is preserved')
            end if
        end if
        configurations = caller_configurations()
        call value_source(2)
        call wait_green(original)
        call assert_true(caller_configurations() == configurations, &
            'a source-only edit reuses the captured CMake configuration')
        call assert_contains(read_text(scratch//'/observed'), '9', 'runs edited code')
        if (len(root_a) > 0 .and. len(root_b) > 0) then
            call fs_stat(root_a//object, after, bytes, ok)
            call assert_true(ok .and. after == before_a, 'idle object is reused after edit')
            call fs_stat(root_b//object, after, bytes, ok)
            call assert_true(ok .and. after == before_b, 'active object remains unchanged')
        end if
        original = gremlin_field(reply, 'active_generation')
        call write_text(project//'/value.f90', 'deliberate compiler error'//nl)
        do attempt = 1, 600
            call status_now()
            if (gremlin_field(reply, 'state') == 'build_failed') exit
            call gremlin_wait_ms(50)
        end do
        call assert_true(gremlin_field(reply, 'state') == 'build_failed', &
            'reports the real compiler failure')
        call assert_true(gremlin_field(reply, 'active_generation') == original, &
            'failing build does not replace the last compilable generation')
        call assert_contains(read_text(scratch//'/observed'), '9', &
            'failed source never runs')
        call value_source(3)
        call wait_green(original)
        call assert_contains(read_text(scratch//'/observed'), '10', 'recovers after failure')
        call gremlin_stop_lane(driver, project, cache, state, lane, session)
    end do
    call finish_assertions(retain_failed_scratch=.true.)
contains
    integer function caller_configurations() result(n)
        character(:), allocatable :: text, line
        integer :: at
        text = read_text(scratch//'/roots')
        line = project//nl
        n = 0
        do
            at = index(text, line)
            if (at == 0) exit
            n = n + 1
            text = text(at + len(line):)
        end do
    end function caller_configurations

    subroutine value_source(value)
        integer, intent(in) :: value
        character(len=12) :: number
        write(number, '(i0)') value
        call write_text(project//'/value.f90', &
            'module value_mod'//nl//'contains'//nl// &
            'integer function value()'//nl//'value='//trim(number)//nl// &
            'end function'//nl//'end module'//nl)
    end subroutine value_source

    subroutine status_now()
        args = string_list_t()
        call list_add(args, 'gremlin')
        call list_add(args, 'status')
        call list_add(args, '--dir')
        call list_add(args, project)
        call list_add(args, '--lane')
        call list_add(args, lane)
        call list_add(args, '--session')
        call list_add(args, session)
        call gremlin_json(driver, project, cache, state, args, reply, process, 10000)
    end subroutine status_now

    subroutine wait_green(previous)
        character(len=*), intent(in) :: previous
        integer :: poll
        do poll = 1, 600
            call status_now()
            if (json_boolean_value(json_member(reply, 'local_gate_green')) .and. &
                gremlin_field(reply, 'active_generation') /= previous) return
            if (gremlin_field(reply, 'state') == 'error') exit
            call gremlin_wait_ms(50)
        end do
        call assert_true(.false., 'new generation reaches a green gate: '// &
            gremlin_field(reply, 'diagnostic'))
    end subroutine wait_green
end program test_gremlin_incremental
