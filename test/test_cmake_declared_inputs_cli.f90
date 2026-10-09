program test_cmake_declared_inputs_cli
    use fo_test_harness, only: string_list_t, process_result_t, list_add, &
        write_text, make_directory, make_symlink, assert_true, assert_contains, &
        assert_process_ok, run_process, finish_assertions
    use fo_test_cli, only: parse_json_report
    use fo_test_gremlin_oracle, only: gremlin_setup, gremlin_start_args, gremlin_run, &
        gremlin_json, gremlin_field, gremlin_wait_ms, gremlin_stop_lane
    use fo_test_json, only: json_value_t, json_member, json_boolean_value
    implicit none
    character(:), allocatable :: driver, scratch, project, cache, state, owner
    character(:), allocatable :: provider, sibling, key, extra, generation, diagnostic
    character(len=*), parameter :: nl = new_line('a')
    type(string_list_t) :: args, environment
    type(process_result_t) :: process
    type(json_value_t) :: reply
    integer :: kind, attempt

    do kind = 1, 4
        call gremlin_setup(driver, scratch, project, cache, state)
        provider = scratch//'/payload'
        sibling = scratch//'/sibling'
        call make_directory(provider)
        call make_directory(sibling)
        call write_text(provider//'/value.f90', 'module payload'//nl// &
            'integer,parameter::value=19'//nl//'end module'//nl)
        call write_text(sibling//'/unused.f90', 'module unused'//nl// &
            'integer,parameter::value=41'//nl//'end module'//nl)
        call write_text(project//'/main.f90', 'program main'//nl//'use payload'//nl// &
            'if(value/=19)error stop "independent payload19"'//nl//'end program'//nl)
        key = 'PAYLOAD_SOURCE_DIR'
        if (kind == 1) key = 'PAYLOAD_SOURCE'
        extra = ''
        if (kind == 3) extra = ' "${CMAKE_CURRENT_SOURCE_DIR}/../sibling/unused.f90"'
        if (kind == 4) then
            call make_symlink('../sibling/unused.f90', project//'/alias.f90')
            extra = ' alias.f90'
        end if
        call write_text(project//'/CMakeLists.txt', &
            'cmake_minimum_required(VERSION 3.24)'//nl// &
            'project(root_contract LANGUAGES Fortran)'//nl//'include(CTest)'//nl// &
            'set('//key//' "${CMAKE_CURRENT_SOURCE_DIR}/../payload" CACHE PATH '// &
            '"Source input")'//nl//'add_executable(oracle main.f90 "${'//key// &
            '}/value.f90"'//extra//')'//nl//'add_test(NAME oracle COMMAND oracle)'//nl)
        call native_reference()
        environment = string_list_t()
        call list_add(environment, 'FO_BACKEND=cmake')
        call list_add(environment, 'FO_CMAKE_CONFIGURE_PRESET=')
        call list_add(environment, 'FO_CMAKE_BUILD_PRESET=')
        call list_add(environment, 'FO_CMAKE_TEST_PRESET=')
        call list_add(environment, 'FO_CMAKE_BUILD_DIR=build')
        call list_add(environment, 'FO_CMAKE_BUILD_TARGETS=oracle')
        args = string_list_t()
        call gremlin_start_args(args, project, 'declared-inputs', 'oracle')
        call list_add(args, '--random')
        call list_add(args, '0')
        call gremlin_run(driver, project, cache, state, args, process, &
            environment, 20000)
        call parse_json_report(process, reply, 'CMake declared-input start')
        call assert_process_ok(process, 'declared input session starts')
        owner = gremlin_field(reply, 'session_id')
        do attempt = 1, 400
            call status()
            if (gremlin_field(reply, 'state') == 'quiescent') exit
            if (gremlin_field(reply, 'state') == 'error') exit
            if (gremlin_field(reply, 'state') == 'capture_failed') exit
            call gremlin_wait_ms(50)
        end do
        if (kind == 2) then
            call assert_true(json_boolean_value(json_member(reply, &
                'local_gate_green')), &
                'declared external source directory runs its captured payload')
            generation = gremlin_field(reply, 'active_generation')
            call gremlin_stop_lane(driver, project, cache, state, &
                'declared-inputs', owner)
            call write_text(provider//'/value.f90', 'module payload'//nl// &
                'integer,parameter::value=31'//nl//'end module'//nl)
            args = string_list_t()
            call list_add(args, 'gremlin')
            call list_add(args, 'reproduce')
            call list_add(args, '--dir')
            call list_add(args, project)
            call list_add(args, '--lane')
            call list_add(args, 'declared-inputs')
            call list_add(args, '--session')
            call list_add(args, owner)
            call list_add(args, '--case')
            call list_add(args, 'oracle')
            call list_add(args, '--generation')
            call list_add(args, generation)
            call gremlin_json(driver, project, cache, state, args, reply, &
                process, 20000)
            call assert_process_ok(process, &
                'frozen declared-root replay after owner stop')
            call assert_contains(process%stdout, 'PASS', &
                'frozen payload19 survives live provider edit to31')
        else
            diagnostic = gremlin_field(reply, 'diagnostic')
            if (kind == 4) then
                call assert_contains(diagnostic, &
                    'referenced CMake input is missing or unsupported', &
                    'referenced external alias is rejected explicitly')
                call assert_contains(diagnostic, 'alias.f90', &
                    'rejected alias is identified')
            else
                call assert_contains(diagnostic, &
                    'unsupported uncaptured external CMake input', &
                    'undeclared authored source is rejected explicitly')
                if (kind == 1) call assert_contains(diagnostic, 'payload', &
                    'missing directory declaration identifies provider')
                if (kind == 3) call assert_contains(diagnostic, 'sibling', &
                    'source declaration never grants access to a sibling')
            end if
            call gremlin_stop_lane(driver, project, cache, state, &
                'declared-inputs', owner, &
                allow_terminal_error=.true.)
        end if
    end do
    call finish_assertions()
contains
    subroutine status()
        type(string_list_t) :: query
        call list_add(query, 'gremlin')
        call list_add(query, 'status')
        call list_add(query, '--dir')
        call list_add(query, project)
        call list_add(query, '--lane')
        call list_add(query, 'declared-inputs')
        call list_add(query, '--session')
        call list_add(query, owner)
        call gremlin_json(driver, project, cache, state, query, reply, process, 10000)
    end subroutine status

    subroutine native_reference()
        type(string_list_t) :: command
        type(process_result_t) :: result
        call list_add(command, 'cmake')
        call list_add(command, '-S')
        call list_add(command, '.')
        call list_add(command, '-B')
        call list_add(command, 'build/reference')
        call run_process(command, project, result)
        call assert_process_ok(result, 'native CMake accepts authored source fixture')
        command = string_list_t()
        call list_add(command, 'cmake')
        call list_add(command, '--build')
        call list_add(command, 'build/reference')
        call run_process(command, project, result)
        call assert_process_ok(result, 'native fixture independently builds')
        command = string_list_t()
        call list_add(command, 'ctest')
        call list_add(command, '--test-dir')
        call list_add(command, 'build/reference')
        call run_process(command, project, result)
        call assert_process_ok(result, 'native CTest independently verifies payload19')
    end subroutine native_reference
end program test_cmake_declared_inputs_cli
