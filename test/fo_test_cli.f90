module fo_test_cli
    use, intrinsic :: iso_c_binding, only: c_int64_t
    use fo_test_harness, only: string_list_t, process_result_t, list_add, list_extend
    use fo_test_harness, only: list_with_first, run_process, assert_true
    use fo_test_harness, only: make_scratch, join_path, make_symlink, remove_tree
    use fo_test_harness, only: file_exists, remove_path, read_text, sleep_ms
    use fo_test_process_identity, only: mcp_kill_owned_tree, mcp_process_identity_running
    use fo_fs, only: fs_is_windows
    use fo_test_harness, only: assert_equal_string, assert_equal_integer
    use fo_test_harness, only: process_alive, start_sentinel, stop_sentinel
    use fo_test_harness, only: exercise_failure_cleanup_probe, open_descriptor_count
    use fo_test_harness, only: assert_contains, assert_process_ok, resolve_test_executable
    use fo_test_json, only: json_value_t, json_parse, json_member, json_element
    use fo_test_json, only: json_string_value, json_number_value, json_boolean_value
    use fo_test_json, only: json_null
    implicit none
    private

    public :: resolve_driver, run_fo, run_external, parse_json_report
    public :: run_arguments
    public :: exercise_harness

contains

    subroutine resolve_driver(driver, explicit)
        character(:), allocatable, intent(out) :: driver
        character(len=*), optional, intent(in) :: explicit
        integer :: argument_count, argument_length, env_length, status

        if (present(explicit)) then
            if (len_trim(explicit) > 0) then
                driver = trim(explicit)
                return
            end if
        end if
        argument_count = command_argument_count()
        if (argument_count > 0) then
            call get_command_argument(1, length=argument_length, status=status)
            if (status == 0 .and. argument_length > 0) then
                allocate(character(len=argument_length) :: driver)
                call get_command_argument(1, value=driver, status=status)
                if (status == 0) return
                deallocate(driver)
            end if
        end if
        call get_environment_variable('FO', length=env_length, status=status)
        if (status == 0 .and. env_length > 0) then
            allocate(character(len=env_length) :: driver)
            call get_environment_variable('FO', value=driver, status=status)
            if (status == 0) then
                driver = trim(driver)
                return
            end if
            deallocate(driver)
        end if
        driver = 'fo'
    end subroutine resolve_driver

    subroutine run_arguments(executable, arguments, cwd, result, environment, timeout_ms)
        character(len=*), intent(in) :: executable, cwd
        type(string_list_t), intent(in) :: arguments
        type(process_result_t), intent(out) :: result
        type(string_list_t), optional, intent(in) :: environment
        integer, optional, intent(in) :: timeout_ms
        type(string_list_t) :: command

        call list_with_first(executable, arguments, command)
        call run_process(command, cwd, result, environment, timeout_ms=timeout_ms)
    end subroutine run_arguments

    subroutine run_external(executable, arguments, cwd, result, environment, timeout_ms)
        character(len=*), intent(in) :: executable, cwd
        type(string_list_t), intent(in) :: arguments
        type(process_result_t), intent(out) :: result
        type(string_list_t), optional, intent(in) :: environment
        integer, optional, intent(in) :: timeout_ms

        call run_arguments(executable, arguments, cwd, result, environment, timeout_ms)
    end subroutine run_external

    subroutine run_fo(driver, arguments, cwd, cache_directory, result, extra_environment, &
            timeout_ms)
        character(len=*), intent(in) :: driver, cwd, cache_directory
        type(string_list_t), intent(in) :: arguments
        type(process_result_t), intent(out) :: result
        type(string_list_t), optional, intent(in) :: extra_environment
        integer, optional, intent(in) :: timeout_ms
        type(string_list_t) :: environment

        call list_add(environment, 'FO_DISABLE_SELF_REFRESH=1')
        call list_add(environment, 'FO_SELF_REFRESH=0')
        call list_add(environment, 'FO_CACHE_DIR=' // trim(cache_directory))
        if (present(extra_environment)) call list_extend(environment, extra_environment)
        call run_arguments(driver, arguments, cwd, result, environment, timeout_ms)
    end subroutine run_fo

    subroutine parse_json_report(result, report, context)
        type(process_result_t), intent(in) :: result
        type(json_value_t), intent(out) :: report
        character(len=*), intent(in) :: context
        logical :: valid
        character(:), allocatable :: error_message

        call json_parse(result%stdout, report, valid, error_message)
        if (.not. valid) then
            call assert_true(.false., trim(context) // ': invalid JSON: ' // error_message)
        end if
    end subroutine parse_json_report

    subroutine exercise_harness()
        type(string_list_t) :: arguments, environment, command
        type(process_result_t) :: child
        type(json_value_t) :: document, field, item
        character(:), allocatable :: scratch, large_text, error_message
        character(:), allocatable :: cleanup_marker, executable, heartbeat, before
        logical :: valid
        logical :: sentinel_survived
        integer :: sentinel_id, descriptor_count, repeat_index, pid, unit, status
        integer(c_int64_t) :: birth

        call make_scratch('fo harness ; $(no-shell)', scratch)
        call resolve_test_executable(executable, valid)
        call assert_true(valid, 'resolve independent native helper executable')
        if (.not. valid) return
        arguments = helper_command(executable, 'print')
        call list_add(arguments, 'argument with spaces ; $(touch no-such-file) "quoted"')
        call run_process(arguments, scratch, child)
        call assert_process_ok(child, 'argv launch with shell metacharacters')
        call assert_equal_string(child%stdout, &
            'argument with spaces ; $(touch no-such-file) "quoted"', &
            'argv boundaries preserve literal spaces and metacharacters')
        call assert_true(.not. file_exists(join_path(scratch, 'no-such-file')), &
            'argv text is never evaluated by a shell')

        arguments = helper_command(executable, 'print')
        large_text = repeat('x', 24000)
        call list_add(arguments, large_text)
        call run_process(arguments, scratch, child)
        call assert_process_ok(child, 'large stdout process')
        call assert_equal_string(child%stdout, large_text, 'large output is byte-exact')

        arguments = helper_command(executable, 'env')
        call list_add(environment, 'FO_TEST_VALUE=environment ; literal')
        call run_process(arguments, scratch, child, environment)
        call assert_process_ok(child, 'child environment override')
        call assert_equal_string(child%stdout, 'environment ; literal', &
            'child environment override preserves punctuation')

        arguments = helper_command(executable, 'streams')
        call run_process(arguments, scratch, child)
        call assert_equal_string(child%stdout, 'out', 'stdout is captured independently')
        call assert_equal_string(child%stderr, 'err', 'stderr is captured independently')
        call assert_equal_integer(child%exit_code, 7, 'nonzero child status is retained')
        call assert_equal_integer(child%term_signal, 0, 'normal nonzero exit has no signal')

        arguments = helper_command(executable, 'sleep')
        call run_process(arguments, scratch, child, timeout_ms=40)
        call assert_true(child%timed_out .and. child%reaped, 'timeout stops and reaps a live child')

        heartbeat = join_path(scratch, 'silent-descendant.heartbeat')
        arguments = helper_command(executable, 'silent-parent')
        call list_add(arguments, heartbeat)
        call run_process(arguments, scratch, child, timeout_ms=500)
        call assert_true(child%timed_out .and. child%reaped .and. .not. child%runner_failed, &
            'timeout drains parent and silent TERM-resistant descendant')
        if (fs_is_windows()) then
            call assert_true(child%elapsed_ms < 4000, 'native drainage finishes within its grace bound')
        else
            call assert_true(child%elapsed_ms < 1500, 'cleanup does not wait for a surviving descendant')
        end if
        call assert_true(file_exists(heartbeat), 'silent descendant ran before timeout')
        pid = -1
        birth = 0
        open(newunit=unit, file=heartbeat//'.identity', status='old', action='read', iostat=status)
        if (status == 0) then
            read(unit, *, iostat=status) pid, birth
            close(unit)
        end if
        call assert_true(status == 0 .and. pid > 0 .and. birth > 0, &
            'silent descendant publishes exact independently observed identity')
        before = read_text(heartbeat)
        call assert_true(len(before) > 0, 'silent descendant produced independent evidence')
        call sleep_ms(300)
        call assert_equal_string(read_text(heartbeat), before, &
            'timeout stops the silent descendant heartbeat')

        if (pid > 0 .and. birth > 0) then
            call assert_true(.not. mcp_process_identity_running(pid, birth), &
                'timeout leaves no live silent descendant identity')
            if (mcp_process_identity_running(pid, birth)) call mcp_kill_owned_tree(pid, birth, status)
        end if

        descriptor_count = open_descriptor_count()
        call assert_true(descriptor_count >= 0, 'inspect native process resources')
        call start_sentinel(sentinel_id)
        arguments = helper_command(executable, 'overflow')
        do repeat_index = 1, 4
            call run_process(arguments, scratch, child, max_output_bytes=8192_c_int64_t)
            call assert_true(child%runner_failed, 'output cap returns runner failure')
            call assert_true(child%reaped, 'output cap drains each owned child')
        end do
        sentinel_survived = process_alive(sentinel_id)
        call stop_sentinel(sentinel_id)
        call assert_true(.not. process_alive(child%process_id), 'output cap leaves no live owned child')
        call assert_true(sentinel_survived, 'runner cleanup preserves unrelated child')
        call assert_equal_integer(int(open_descriptor_count()), descriptor_count, &
            'repeated runner failures close every native pipe and process resource')

        command = helper_command(executable, 'stdin')
        call run_process(command, scratch, child, input='stdin ; literal'//achar(0)//'tail')
        call assert_process_ok(child, 'child stdin delivery')
        call assert_equal_string(child%stdout, 'stdin ; literal'//achar(0)//'tail', &
            'child stdin preserves every byte including NUL')

        call json_parse('{"text":"quote=\" slash=\\ line=\n snowman=\u2603",' // &
            '"items":[true,false,null,-1.25e+2]}', document, valid, error_message)
        call assert_true(valid, 'independent JSON parser accepts RFC 8259 escapes')
        field = json_member(document, 'text')
        call assert_equal_string(json_string_value(field), &
            'quote=" slash=\ line=' // achar(10) // ' snowman=' // char(226) // &
            char(152) // char(131), 'JSON escapes and Unicode decode independently')
        field = json_member(document, 'items')
        item = json_element(field, 4)
        call assert_equal_integer(nint(json_number_value(item)), -125, &
            'JSON exponent numbers retain their numeric value')
        item = json_element(field, 1)
        call assert_true(json_boolean_value(item), 'JSON booleans parse independently')
        item = json_element(field, 3)
        call assert_equal_integer(item%kind, json_null, 'JSON null parses independently')
        call json_parse('[1,]', document, valid, error_message)
        call assert_true(.not. valid, 'independent JSON parser rejects trailing array comma')
        call json_parse('{"bad":"\q"}', document, valid, error_message)
        call assert_true(.not. valid, 'independent JSON parser rejects invalid escapes')
        call json_parse('01', document, valid, error_message)
        call assert_true(.not. valid, 'independent JSON parser rejects leading zeroes')

        cleanup_marker = join_path(scratch, 'cleanup-probe-path')
        call exercise_failure_cleanup_probe(cleanup_marker)
        call remove_path(cleanup_marker)
        call remove_tree(scratch)
    end subroutine exercise_harness
    function helper_command(executable, mode) result(command)
        character(len=*), intent(in) :: executable, mode
        type(string_list_t) :: command
        call list_add(command, executable)
        call list_add(command, '--fo-test-helper')
        call list_add(command, mode)
    end function helper_command

end module fo_test_cli
