program test_cmake_scope_cli
    use fo_test_harness, only: string_list_t, process_result_t, list_add, &
        write_text, read_text, make_directory, file_exists, assert_true, &
        assert_equal_string, &
        assert_process_ok, assert_equal_integer, run_process, finish_assertions
    use fo_test_gremlin_oracle, only: gremlin_setup, gremlin_start_args, &
        gremlin_json, gremlin_run, gremlin_field, gremlin_wait_ms, gremlin_stop_lane
    use fo_test_cli, only: parse_json_report
    use fo_test_json, only: json_value_t, json_member, json_boolean_value, &
        json_string_value, json_number_value
    implicit none
    character(:), allocatable :: driver, scratch, project, cache, state, owner
    character(:), allocatable :: generation, previous, leaf, counted_runs
    character(len=*), parameter :: nl = new_line('a')
    type(string_list_t) :: args, environment
    type(process_result_t) :: process
    type(json_value_t) :: reply
    integer :: phase, completed

    call gremlin_setup(driver, scratch, project, cache, state)
    call make_directory(project//'/out/authored')
    leaf = project//'/out/authored/value.inc'
    call write_text(leaf, 'integer,parameter::value=23'//nl)
    call write_text(project//'/main.f90', 'program gate'//nl// &
        'include "out/authored/value.inc"'//nl// &
        'integer::unit'//nl//'open(newunit=unit, &'//nl// &
        'file="'//scratch(:len(scratch)/2)//'"// &'//nl// &
        '"'//scratch(len(scratch)/2 + 1:)//'/gate-runs", &'//nl// &
        'status="unknown",position="append")'//nl// &
        'write(unit,*)value'//nl//'close(unit)'//nl// &
        'if(value/=17)error stop "independent oracle17"'//nl// &
        'print *,"oracle17"'//nl//'end program'//nl)
    call write_text(project//'/invalid.f90', 'unselected target must never compile')
    call write_text(project//'/CMakeLists.txt', &
        'cmake_minimum_required(VERSION 3.20)'//nl// &
        'project(scope_oracle LANGUAGES Fortran)'//nl//'enable_testing()'//nl// &
        'add_executable(gate main.f90)'//nl// &
        'add_executable(unbuilt invalid.f90)'//nl// &
        'target_include_directories(gate PRIVATE "${CMAKE_CURRENT_SOURCE_DIR}")'//nl// &
        'add_test(NAME selected_red COMMAND gate)'//nl// &
        'add_test(NAME selected_companion COMMAND "${CMAKE_COMMAND}" -E touch "'// &
        scratch//'/companion.ran")'//nl// &
        'add_test(NAME unrelated_sentinel COMMAND "${CMAKE_COMMAND}" -E touch "'// &
        scratch//'/unrelated.ran")'//nl// &
        'add_test(NAME unrelated_unbuilt COMMAND unbuilt)'//nl)
    call write_text(project//'/CMakePresets.json', &
        '{"version":3,"configurePresets":[{"name":"base","hidden":true,'// &
        '"generator":"Ninja","binaryDir":"${sourceDir}/out/native",'// &
        '"cacheVariables":{"CMAKE_BUILD_TYPE":"Release"}},'// &
        '{"name":"cpu","inherits":"base"}],'// &
        '"buildPresets":[{"name":"cpu","configurePreset":"cpu"}],'// &
        '"testPresets":[{"name":"cpu","configurePreset":"cpu"}]}')
    call list_add(environment, 'FO_BACKEND=cmake')
    call list_add(environment, 'FO_CMAKE_CONFIGURE_PRESET=cpu')
    call list_add(environment, 'FO_CMAKE_BUILD_PRESET=cpu')
    call list_add(environment, 'FO_CMAKE_TEST_PRESET=cpu')
    call list_add(environment, 'FO_CMAKE_BUILD_TARGETS=gate')
    call list_add(environment, 'FO_CMAKE_BUILD_DIR=')
    call native_reference()
    call gremlin_start_args(args, project, 'scope', 'selected_red')
    call list_add(args, '--target')
    call list_add(args, 'selected_companion')
    call list_add(args, '--random')
    call list_add(args, '0')
    call gremlin_run(driver, project, cache, state, args, process, environment, 20000)
    call parse_json_report(process, reply, 'CMake resident start report')
    call assert_process_ok(process, 'native inherited CMake preset starts resident')
    owner = gremlin_field(reply, 'session_id')
    previous = ''
    do phase = 1, 3
        if (phase == 2) call write_text(leaf, 'integer,parameter::value=17'//nl)
        if (phase == 3) call write_text(leaf, 'integer,parameter::value=23'//nl)
        call wait_settled(phase == 2)
        generation = gremlin_field(reply, 'active_generation')
        call assert_true(len(generation) == 64 .and. generation /= previous, &
            'authored sibling edits wake a new exact CMake generation')
        call assert_true(file_exists(scratch//'/companion.ran'), &
            'failed required case still allows selected companion to execute')
        call assert_true(.not. file_exists(scratch//'/unrelated.ran'), &
            'zero random selection never executes the unrelated registered case')
        completed = int(json_number_value(json_member(reply, 'completed')))
        if (phase == 1) then
            call assert_equal_integer(completed, 2, &
                'only explicitly required cases complete initially')
        else
            call assert_true(completed >= 2 .and. completed <= 2*phase, &
                'edited generations retain finite selected-case completion')
        end if
        if (phase /= 2) call assert_equal_string(gremlin_field(reply, 'health'), &
            'failure', 'settled numerical failure retains truthful health')
        counted_runs = read_text(scratch//'/gate-runs')
        call gremlin_wait_ms(500)
        call query_status()
        call assert_equal_integer( &
            int(json_number_value(json_member(reply, 'completed'))), &
            completed, &
            'settled failed gates retain finite completion without rerunning')
        call assert_equal_string(read_text(scratch//'/gate-runs'), counted_runs, &
            'quiescence runs no hidden numerical retry campaigns')
        call write_text(project//'/out/native/generated.f90', 'generated output')
        call gremlin_wait_ms(500)
        call query_status()
        call assert_equal_string(gremlin_field(reply, 'active_generation'), &
            generation, &
            'custom CMake build outputs never create authored generations')
        previous = generation
    end do
    call gremlin_stop_lane(driver, project, cache, state, 'scope', owner)
    call finish_assertions()
contains
    subroutine native_reference()
        type(string_list_t) :: command
        type(process_result_t) :: observed
        call list_add(command, 'cmake')
        call list_add(command, '--preset')
        call list_add(command, 'cpu')
        call run_process(command, project, observed)
        call assert_process_ok(observed, &
            'independent native inherited configure preset')
        command = string_list_t()
        call list_add(command, 'cmake')
        call list_add(command, '--build')
        call list_add(command, '--preset')
        call list_add(command, 'cpu')
        call list_add(command, '--target')
        call list_add(command, 'gate')
        call run_process(command, project, observed)
        call assert_process_ok(observed, 'native selected build avoids invalid target')
        command = string_list_t()
        call list_add(command, 'ctest')
        call list_add(command, '--preset')
        call list_add(command, 'cpu')
        call list_add(command, '-R')
        call list_add(command, '^selected_red$')
        call run_process(command, project, observed)
        call assert_true(observed%exit_code /= 0, &
            'native CTest independently confirms initial numerical red verdict')
    end subroutine native_reference

    subroutine query_status()
        type(string_list_t) :: query
        call list_add(query, 'gremlin')
        call list_add(query, 'status')
        call list_add(query, '--dir')
        call list_add(query, project)
        call list_add(query, '--lane')
        call list_add(query, 'scope')
        call list_add(query, '--session')
        call list_add(query, owner)
        call gremlin_json(driver, project, cache, state, query, reply, process, 10000)
    end subroutine query_status

    subroutine wait_settled(green)
        logical, intent(in) :: green
        type(json_value_t) :: failure
        logical :: settled
        integer :: attempt
        settled = .false.
        do attempt = 1, 400
            call query_status()
            if (gremlin_field(reply, 'state') == 'error') exit
            if (gremlin_field(reply, 'state') == 'quiescent') then
                settled = json_boolean_value(json_member(reply, 'local_gate_green')) &
                    .eqv. green
                if (.not. green) then
                    failure = json_member(reply, 'latest_failure')
                    settled = settled .and. &
                        json_string_value(json_member(failure, 'case_id')) == &
                        'selected_red' .and. &
                        json_string_value(json_member(failure, 'generation')) == &
                        gremlin_field(reply, 'active_generation')
                end if
                if (settled) exit
            end if
            call gremlin_wait_ms(50)
        end do
        if (.not. settled) write(*, '(a)') process%stdout
        call assert_true(settled, 'selected CTest red/green gate reaches quiescence')
    end subroutine wait_settled
end program test_cmake_scope_cli
