module fo_mcp
    use fo_util, only: json_bool, json_int, extract_json_field, make_tmpfile, &
        delete_tmpfile, read_text_file, clean_root_build_artifacts, &
        jsonrpc_error, jsonrpc_null, strip_path_prefix_in_str
    use fx_json_build, only: json_escape_string
    use fx_mcp, only: mcp_read_message, mcp_send_response, MCP_FRAME_UNKNOWN
    use fo_fs, only: fs_remove_tree
    use fo_check, only: check_result_t, fo_check_run
    use fo_check_output, only: check_result_compact_json, &
        check_result_full_json
    use fo_fmt, only: fo_fmt_run
    use fo_capabilities, only: capabilities_t, detect_capabilities, &
        capabilities_json
    use fo_process, only: process_start_fo_check, process_poll_pid, &
        process_cancel_pid
    use fo_progress, only: progress_suppress
    use fo_run_queue, only: run_queue_t, RUN_IDLE, RUN_RUNNING, &
        RUN_RERUN_PENDING
    use fo_build_backend, only: backend_t, detect_backend, BACKEND_NONE
    implicit none
    private
    public :: mcp_serve

    integer, parameter :: MAX_LINE = 32768

    type :: mcp_async_state_t
        type(run_queue_t) :: queue
        integer :: active_pid = 0
        integer :: active_run_id = 0
        integer :: pending_run_id = 0
        integer :: last_run_id = 0
        integer :: last_exitcode = 0
        integer :: next_run_id = 0
        character(len=512) :: active_output = ''
        character(len=512) :: last_output = ''
    end type mcp_async_state_t

contains

    subroutine mcp_serve()
        character(len=MAX_LINE) :: line, response
        character(len=256) :: method, id_str
        integer :: framing, read_status
        logical :: eof_flag
        type(mcp_async_state_t) :: async_state

        framing = MCP_FRAME_UNKNOWN
        do
            call mcp_read_message(line, MAX_LINE, framing, eof_flag, read_status)
            if (eof_flag) exit
            if (read_status /= 0) cycle
            if (len_trim(line) == 0) cycle

            call extract_json_field(line, '"method"', method)
            call extract_json_field(line, '"id"', id_str)
            call async_poll(async_state)

            select case (trim(method))
            case ('initialize')
                call make_initialize_response(id_str, line, response)
                call mcp_send_response(trim(response), framing)
            case ('initialized')
                ! notification, no response needed
                cycle
            case ('tools/list')
                call make_tools_list_response(id_str, response)
                call mcp_send_response(trim(response), framing)
            case ('tools/call')
                call handle_tools_call(line, id_str, response, async_state)
                call mcp_send_response(trim(response), framing)
            case ('resources/list')
                call make_resources_list_response(id_str, response)
                call mcp_send_response(trim(response), framing)
            case ('resources/read')
                call handle_resources_read(line, id_str, response, async_state)
                call mcp_send_response(trim(response), framing)
            case ('shutdown')
                call async_cancel_all(async_state)
                call jsonrpc_null(id_str, response)
                call mcp_send_response(trim(response), framing)
                exit
            case default
                if (len_trim(id_str) > 0) then
                    call jsonrpc_error(id_str, -32601, &
                        'method not found', response)
                    call mcp_send_response(trim(response), framing)
                end if
            end select
        end do
        call async_cancel_all(async_state)
    end subroutine mcp_serve

    subroutine handle_tools_call(line, id_str, response, async_state)
        character(len=*), intent(in) :: line, id_str
        character(len=*), intent(out) :: response
        type(mcp_async_state_t), intent(inout) :: async_state

        character(len=64) :: action, mode
        character(len=16384) :: output_text
        integer :: exitcode
        character(len=512) :: tmpfile, dir
        type(check_result_t) :: check_res

        call extract_json_field(line, '"action"', action)
        call extract_json_field(line, '"dir"', dir)
        if (len_trim(dir) == 0) dir = '.'
        call make_tmpfile('fo_mcp_output', tmpfile)

        select case (trim(action))
        case ('check')
            call extract_json_field(line, '"mode"', mode)
            if (trim(mode) == 'start') then
                call handle_async_start(line, id_str, response, async_state)
                return
            end if
            call handle_check(line, id_str, dir, check_res, output_text, &
                exitcode, response)
        case ('status')
            call handle_async_status(id_str, response, async_state)
            return
        case ('diagnostics')
            call handle_async_diagnostics(line, id_str, response, async_state)
            return
        case ('cancel')
            call handle_async_cancel(line, id_str, response, async_state)
            return
        case ('lint')
            call handle_lint(line, id_str, dir, output_text, exitcode, response)
        case ('fmt')
            call fo_fmt_run(trim(dir), exitcode)
            if (exitcode == 0) then
                output_text = 'formatted'
            else
                output_text = 'fo fmt: formatting failed'
            end if
            call make_tool_text_response(id_str, output_text, exitcode, response)
        case ('build')
            call handle_backend_build(id_str, dir, tmpfile, response)
            return
        case ('test')
            call handle_backend_test(id_str, dir, tmpfile, response)
            return
        case ('graph')
            call handle_graph(line, id_str, dir, output_text, exitcode, response)
        case ('info')
            call handle_info(id_str, dir, output_text, exitcode, response)
        case ('changed')
            call handle_changed(id_str, dir, output_text, exitcode, response)
        case ('clean')
            call handle_clean(id_str, dir, output_text, exitcode, response)
        case default
            call jsonrpc_error(id_str, -32602, &
                'unknown action: '//trim(action), response)
        end select
        call delete_tmpfile(tmpfile)
    end subroutine handle_tools_call

    subroutine handle_check(line, id_str, dir, check_res, output_text, &
            exitcode, response)
        character(len=*), intent(in) :: line, id_str, dir
        type(check_result_t), intent(out) :: check_res
        character(len=*), intent(out) :: output_text
        integer, intent(out) :: exitcode
        character(len=*), intent(out) :: response

        logical :: want_full
        type(capabilities_t) :: cap
        character(len=2048) :: cap_json
        character(len=514) :: dir_prefix
        want_full = (index(line, '"full"') > 0 .or. &
            index(line, '"json":"full"') > 0)
        cap_json = ''
        if (want_full) then
            call detect_capabilities(cap)
            call capabilities_json(cap, cap_json)
        end if
        call progress_suppress(.true.)
        call fo_check_run(trim(dir), check_res)
        call progress_suppress(.false.)
        if (want_full) then
            output_text = check_result_full_json(check_res, cap_json)
        else
            output_text = check_result_compact_json(check_res)
        end if
        dir_prefix = trim(dir)//'/'
        call strip_path_prefix_in_str(output_text, trim(dir_prefix))
        exitcode = 0
        if (.not. (check_res%build_ok .and. check_res%tests_ok)) exitcode = 1
        call make_tool_text_response(id_str, output_text, exitcode, response)
    end subroutine handle_check

    subroutine handle_lint(line, id_str, dir, output_text, exitcode, response)
        use fo_lint, only: lint_finding_t, lint_warning_t, lint_dir, &
            lint_compiler, lint_dedup_warnings, lint_all_json, &
            lint_fix_dir, MAX_FINDINGS, MAX_WARNINGS
        character(len=*), intent(in) :: line, id_str, dir
        character(len=*), intent(out) :: output_text
        integer, intent(out) :: exitcode
        character(len=*), intent(out) :: response

        type(lint_finding_t) :: findings(MAX_FINDINGS)
        type(lint_warning_t), allocatable :: warnings(:)
        integer :: n_findings, n_warnings, n_removed, n_remaining
        character(len=16384) :: lint_output
        character(len=514) :: dir_prefix
        character(len=16) :: fix_flag

        call extract_json_field(line, '"fix"', fix_flag)
        if (trim(fix_flag) == 'true') then
            call lint_fix_dir(trim(dir), n_removed, n_remaining)
            write (lint_output, '(a,i0,a,i0,a)') &
                '{"removed":', n_removed, ',"remaining":', n_remaining, '}'
            output_text = trim(lint_output)
            exitcode = 0
            if (n_remaining > 0) exitcode = 1
            call make_tool_text_response(id_str, lint_output, exitcode, response)
            return
        end if

        allocate (warnings(MAX_WARNINGS))
        call lint_dir(trim(dir), findings, n_findings)
        call lint_compiler(trim(dir), warnings, n_warnings)
        call lint_dedup_warnings(warnings, n_warnings)
        lint_output = lint_all_json(findings, n_findings, warnings, n_warnings)
        dir_prefix = trim(dir)//'/'
        call strip_path_prefix_in_str(lint_output, trim(dir_prefix))
        output_text = trim(lint_output)
        exitcode = 0
        if (n_findings > 0 .or. n_warnings > 0) exitcode = 1
        call make_tool_text_response(id_str, lint_output, exitcode, response)
    end subroutine handle_lint

    subroutine handle_backend_build(id_str, dir, tmpfile, response)
        use fo_build_backend, only: backend_t, detect_backend, backend_build, &
            BACKEND_NONE
        character(len=*), intent(in) :: id_str, dir, tmpfile
        character(len=*), intent(out) :: response

        type(backend_t) :: b
        character(len=8192) :: output_text
        integer :: exitcode

        b = detect_backend(trim(dir))
        if (b%kind == BACKEND_NONE) then
            output_text = 'fo: no fpm.toml found'
            exitcode = 1
        else
            call backend_build(b, exitcode, log_file=tmpfile)
            call read_text_file(tmpfile, output_text)
        end if
        call delete_tmpfile(tmpfile)
        call make_tool_text_response(id_str, output_text, exitcode, response)
    end subroutine handle_backend_build

    subroutine handle_backend_test(id_str, dir, tmpfile, response)
        use fo_build_backend, only: backend_t, detect_backend, backend_test, &
            BACKEND_NONE
        character(len=*), intent(in) :: id_str, dir, tmpfile
        character(len=*), intent(out) :: response

        type(backend_t) :: b
        character(len=8192) :: output_text
        integer :: exitcode

        b = detect_backend(trim(dir))
        if (b%kind == BACKEND_NONE) then
            output_text = 'fo: no fpm.toml found'
            exitcode = 1
        else
            call backend_test(b, exitcode, log_file=tmpfile)
            call read_text_file(tmpfile, output_text)
        end if
        call delete_tmpfile(tmpfile)
        call make_tool_text_response(id_str, output_text, exitcode, response)
    end subroutine handle_backend_test

    subroutine handle_graph(line, id_str, dir, output_text, exitcode, response)
        use fo_scan, only: scan_unit_t, scan_dir, MAX_UNITS
        use fx_dag, only: dag_t, dag_to_dot
        use fo_dag_bridge, only: build_dag_from_units
        use fo_build_backend, only: backend_t, detect_backend, BACKEND_NONE
        character(len=*), intent(in) :: line, id_str, dir
        character(len=*), intent(out) :: output_text
        integer, intent(out) :: exitcode
        character(len=*), intent(out) :: response

        type(backend_t) :: b
        type(scan_unit_t), allocatable :: units(:)
        type(dag_t) :: dag
        character(len=512) :: scan_root
        integer :: n_units, ierr, i, j
        character(len=:), allocatable :: dot_out

        allocate (units(MAX_UNITS))
        b = detect_backend(trim(dir))
        scan_root = trim(dir)
        if (b%kind /= BACKEND_NONE) scan_root = b%project_dir
        call scan_dir(trim(scan_root), units, n_units, ierr)
        if (ierr /= 0) then
            output_text = 'fo: scan failed'
            exitcode = 1
        else
            call build_dag_from_units(units, n_units, dag)
            exitcode = 0
            if (index(line, '"dot"') > 0) then
                call dag_to_dot(dag, dot_out)
                output_text = trim(dot_out)
            else
                output_text = ''
                do i = 1, dag%n_nodes
                    if (dag%nodes(i)%n_edges == 0) then
                        output_text = trim(output_text)// &
                            trim(dag%nodes(i)%label)//achar(10)
                    else
                        do j = 1, dag%nodes(i)%n_edges
                            output_text = trim(output_text)// &
                                trim(dag%nodes(i)%label)//' -> '// &
                                trim(dag%nodes(dag%nodes(i)%edges(j))%label)//achar(10)
                        end do
                    end if
                end do
            end if
        end if
        call make_tool_text_response(id_str, output_text, exitcode, response)
    end subroutine handle_graph

    subroutine handle_info(id_str, dir, output_text, exitcode, response)
        use fo_scan, only: scan_unit_t, scan_dir, MAX_UNITS
        use fx_dag, only: dag_t
        use fo_dag_bridge, only: build_dag_from_units
        use fo_build_backend, only: backend_t, detect_backend, &
            BACKEND_NONE, BACKEND_NATIVE
        use fo_cache, only: cache_schema, cache_store_root
        character(len=*), intent(in) :: id_str, dir
        character(len=*), intent(out) :: output_text
        integer, intent(out) :: exitcode
        character(len=*), intent(out) :: response

        type(backend_t) :: b
        type(scan_unit_t), allocatable :: units(:)
        type(dag_t) :: dag
        character(len=512) :: scan_root, cache_text
        integer :: n_units, ierr

        allocate (units(MAX_UNITS))
        b = detect_backend(trim(dir))
        scan_root = trim(dir)
        if (b%kind /= BACKEND_NONE) scan_root = b%project_dir
        select case (b%kind)
        case (BACKEND_NATIVE)
            output_text = 'backend: native'//achar(10)
        case default
            output_text = 'backend: none'//achar(10)
        end select
        call cache_schema(cache_text)
        output_text = trim(output_text)//'cache-schema: '// &
            trim(cache_text)//achar(10)
        call cache_store_root(cache_text)
        output_text = trim(output_text)//'cache-root: '// &
            trim(cache_text)//achar(10)//'cache-shards: 256'//achar(10)
        call scan_dir(trim(scan_root), units, n_units, ierr)
        if (ierr == 0) then
            call build_dag_from_units(units, n_units, dag)
            write (output_text(len_trim(output_text) + 1:), '(a,i0)') &
                'files: ', n_units
            output_text = trim(output_text)//achar(10)
            write (output_text(len_trim(output_text) + 1:), '(a,i0)') &
                'modules: ', dag%n_nodes
        end if
        exitcode = 0
        call make_tool_text_response(id_str, output_text, exitcode, response)
    end subroutine handle_info

    subroutine handle_changed(id_str, dir, output_text, exitcode, response)
        use fo_check, only: fo_changed_modules
        use fx_dag, only: dag_t, MAX_NODES
        use fo_scan, only: MAX_PATH
        character(len=*), intent(in) :: id_str, dir
        character(len=*), intent(out) :: output_text
        integer, intent(out) :: exitcode
        character(len=*), intent(out) :: response

        type(dag_t) :: dag
        integer :: changed_ids(MAX_NODES), n_changed
        integer :: affected_ids(MAX_NODES), n_affected
        integer :: n_cached, ierr, i, n_tests
        character(len=MAX_PATH) :: filenames(MAX_NODES)
        logical :: is_test_arr(MAX_NODES)
        call fo_changed_modules(trim(dir), dag, changed_ids, n_changed, &
            affected_ids, n_affected, n_cached, ierr, &
            filenames=filenames, is_test_arr=is_test_arr)
        if (ierr /= 0) then
            output_text = 'fo: scan or dag failed'
            exitcode = 1
            call make_tool_text_response(id_str, output_text, exitcode, response)
            return
        end if
        exitcode = 0
        if (n_changed == 0) then
            write (output_text, '(a,i0,a)') 'all ', n_cached, ' modules cached'
            call make_tool_text_response(id_str, output_text, exitcode, response)
            return
        end if
        write (output_text, '(a,i0,a)') 'changed (', n_changed, '):'
        do i = 1, n_changed
            output_text = trim(output_text)//achar(10)//'  '// &
                trim(dag%nodes(changed_ids(i))%label)//'  '// &
                trim(filenames(changed_ids(i)))
        end do
        output_text = trim(output_text)//achar(10)
        write (output_text(len_trim(output_text) + 1:), '(a,i0,a)') &
            'affected (', n_affected, '):'
        do i = 1, n_affected
            output_text = trim(output_text)//achar(10)//'  '// &
                trim(dag%nodes(affected_ids(i))%label)//'  '// &
                trim(filenames(affected_ids(i)))
        end do
        n_tests = 0
        do i = 1, n_affected
            if (is_test_arr(affected_ids(i))) n_tests = n_tests + 1
        end do
        if (n_tests > 0) then
            output_text = trim(output_text)//achar(10)
            write (output_text(len_trim(output_text) + 1:), '(a,i0,a)') &
                'affected tests (', n_tests, '):'
            do i = 1, n_affected
                if (is_test_arr(affected_ids(i))) then
                    output_text = trim(output_text)//achar(10)//'  '// &
                        trim(dag%nodes(affected_ids(i))%label)//'  '// &
                        trim(filenames(affected_ids(i)))
                end if
            end do
        end if
        call make_tool_text_response(id_str, output_text, exitcode, response)
    end subroutine handle_changed

    subroutine handle_clean(id_str, dir, output_text, exitcode, response)
        use fo_cache, only: cache_root, cache_store_root
        character(len=*), intent(in) :: id_str, dir
        character(len=*), intent(out) :: output_text
        integer, intent(out) :: exitcode
        character(len=*), intent(out) :: response

        character(len=512) :: root, store_root
        type(backend_t) :: b
        integer :: n_removed

        call cache_root(root)
        call cache_store_root(store_root)
        call fs_remove_tree(trim(root))
        b = detect_backend(trim(dir))
        if (b%kind /= BACKEND_NONE) then
            call fs_remove_tree(trim(b%project_dir)//'/build')
            call clean_root_build_artifacts(trim(b%project_dir), n_removed)
        end if
        output_text = 'cache cleared: '//trim(store_root)
        if (b%kind /= BACKEND_NONE) output_text = trim(output_text)//achar(10)// &
            'build tree cleared: '//trim(b%project_dir)//'/build'
        exitcode = 0
        call make_tool_text_response(id_str, output_text, exitcode, response)
    end subroutine handle_clean

    subroutine handle_resources_read(line, id_str, response, async_state)
        character(len=*), intent(in) :: line, id_str
        character(len=*), intent(out) :: response
        type(mcp_async_state_t), intent(inout) :: async_state

        character(len=256) :: uri
        character(len=8192) :: output_text
        integer :: exitcode
        type(check_result_t) :: check_res

        call extract_json_field(line, '"uri"', uri)
        call async_poll(async_state)

        if (trim(uri) == 'fo://diagnostics' .or. &
            index(line, 'fo://diagnostics') > 0) then
            if (len_trim(async_state%last_output) > 0) then
                call read_text_file(async_state%last_output, output_text)
                exitcode = async_state%last_exitcode
            else
                call fo_check_run('.', check_res)
                output_text = check_result_compact_json(check_res)
                exitcode = 0
                if (.not. (check_res%build_ok .and. check_res%tests_ok)) exitcode = 1
            end if

            output_text = json_escape_string(output_text)
            response = '{"jsonrpc":"2.0","id":'//trim(id_str)//','// &
                '"result":{"contents":[{"uri":"fo://diagnostics",'// &
                '"mimeType":"text/plain",'// &
                '"text":"'//trim(output_text)//'"}]}}'
        else
            call jsonrpc_error(id_str, -32602, &
                'unknown resource', response)
        end if
    end subroutine handle_resources_read

    subroutine handle_async_start(line, id_str, response, async_state)
        character(len=*), intent(in) :: line, id_str
        character(len=*), intent(out) :: response
        type(mcp_async_state_t), intent(inout) :: async_state

        character(len=512) :: root
        character(len=32) :: output_mode
        integer :: ierr, run_id, started_before
        logical :: pending

        call extract_json_field(line, '"root"', root)
        if (len_trim(root) == 0) root = '.'
        output_mode = 'agent'
        if (index(line, '"json":"full"') > 0 .or. index(line, '"full"') > 0) then
            output_mode = 'full'
        end if

        started_before = async_state%queue%started
        call async_state%queue%request(root, output_mode, ierr)
        if (ierr /= 0) then
            call jsonrpc_error(id_str, -32602, 'invalid root', response)
            return
        end if

        pending = async_state%queue%started == started_before
        if (pending) then
            async_state%next_run_id = async_state%next_run_id + 1
            async_state%pending_run_id = async_state%next_run_id
            run_id = async_state%pending_run_id
        else
            call async_start_current(async_state, 0, ierr)
            if (ierr /= 0) then
                call jsonrpc_error(id_str, -32603, &
                    'could not start check', response)
                return
            end if
            run_id = async_state%active_run_id
        end if

        call make_run_start_response(id_str, run_id, pending, response)
    end subroutine handle_async_start

    subroutine handle_async_status(id_str, response, async_state)
        character(len=*), intent(in) :: id_str
        character(len=*), intent(out) :: response
        type(mcp_async_state_t), intent(inout) :: async_state

        character(len=1024) :: status_text

        call async_poll(async_state)

        if (async_state%active_pid > 0) then
            status_text = '{"state":"running"'// &
                ',"run_id":'//trim(json_int(async_state%active_run_id))//'}'
        else if (async_state%pending_run_id > 0) then
            status_text = '{"state":"rerun-pending"'// &
                ',"run_id":'//trim(json_int(async_state%pending_run_id))//'}'
        else if (async_state%last_run_id > 0) then
            status_text = '{"state":"finished"'// &
                ',"run_id":'//trim(json_int(async_state%last_run_id))// &
                ',"exitcode":'// &
                trim(json_int(async_state%last_exitcode))//'}'
        else
            status_text = '{"state":"idle"}'
        end if

        call make_tool_text_response(id_str, status_text, 0, response)
    end subroutine handle_async_status

    subroutine handle_async_diagnostics(line, id_str, response, async_state)
        character(len=*), intent(in) :: line, id_str
        character(len=*), intent(out) :: response
        type(mcp_async_state_t), intent(inout) :: async_state

        character(len=8192) :: output_text
        integer :: run_id, ierr

        call async_poll(async_state)
        call requested_run_id(line, async_state, run_id, ierr)
        if (ierr /= 0) then
            call jsonrpc_error(id_str, -32602, 'unknown run_id', response)
            return
        end if

        output_text = ''
        if (run_id > 0 .and. run_id == async_state%last_run_id .and. &
            len_trim(async_state%last_output) > 0) then
            call read_text_file(async_state%last_output, output_text)
        end if

        if (len_trim(output_text) == 0) then
            output_text = '{"state":"idle","diagnostics":""}'
        end if

        call make_tool_text_response(id_str, output_text, 0, response)
    end subroutine handle_async_diagnostics

    subroutine handle_async_cancel(line, id_str, response, async_state)
        character(len=*), intent(in) :: line, id_str
        character(len=*), intent(out) :: response
        type(mcp_async_state_t), intent(inout) :: async_state

        integer :: run_id, ierr, exitcode

        call requested_run_id(line, async_state, run_id, ierr)
        if (ierr /= 0) then
            call jsonrpc_error(id_str, -32602, 'unknown run_id', response)
            return
        end if
        if (run_id /= async_state%active_run_id .or. async_state%active_pid <= 0) then
            call jsonrpc_error(id_str, -32602, 'run is not active', response)
            return
        end if

        call process_cancel_pid(async_state%active_pid, exitcode)
        async_state%active_pid = 0
        call async_state%queue%finish(130)
        async_state%last_run_id = run_id
        async_state%last_exitcode = 130
        async_state%last_output = async_state%active_output
        async_state%active_output = ''
        async_state%active_run_id = 0
        call async_start_pending_if_ready(async_state)

        block
            character(len=256) :: cancel_text
            cancel_text = '{"cancelled":true,"run_id":'// &
                trim(json_int(run_id))//'}'
            call make_tool_text_response(id_str, cancel_text, 0, response)
        end block
    end subroutine handle_async_cancel

    subroutine async_poll(async_state)
        type(mcp_async_state_t), intent(inout) :: async_state

        logical :: done
        integer :: exitcode

        if (async_state%active_pid <= 0) then
            call async_start_pending_if_ready(async_state)
            return
        end if

        call process_poll_pid(async_state%active_pid, done, exitcode)
        if (.not. done) return

        async_state%last_run_id = async_state%active_run_id
        async_state%last_exitcode = exitcode
        async_state%last_output = async_state%active_output
        async_state%active_pid = 0
        async_state%active_run_id = 0
        async_state%active_output = ''
        call async_state%queue%finish(exitcode)
        call async_start_pending_if_ready(async_state)
    end subroutine async_poll

    subroutine async_start_pending_if_ready(async_state)
        type(mcp_async_state_t), intent(inout) :: async_state

        integer :: ierr, run_id

        if (async_state%active_pid > 0) return
        if (async_state%queue%state /= RUN_RUNNING) return

        run_id = async_state%pending_run_id
        async_state%pending_run_id = 0
        call async_start_current(async_state, run_id, ierr)
    end subroutine async_start_pending_if_ready

    subroutine async_start_current(async_state, requested_id, ierr)
        type(mcp_async_state_t), intent(inout) :: async_state
        integer, intent(in) :: requested_id
        integer, intent(out) :: ierr

        integer :: pid, exitcode, run_id
        character(len=512) :: output_file

        ierr = 0
        run_id = requested_id
        if (run_id <= 0) then
            async_state%next_run_id = async_state%next_run_id + 1
            run_id = async_state%next_run_id
        end if

        call make_tmpfile('fo_mcp_async', output_file)
        call process_start_fo_check(async_state%queue%current_root, &
            async_state%queue%current_mode, output_file, &
            pid, exitcode)
        if (exitcode /= 0 .or. pid <= 0) then
            ierr = 1
            return
        end if

        async_state%active_pid = pid
        async_state%active_run_id = run_id
        async_state%active_output = output_file
    end subroutine async_start_current

    subroutine async_cancel_all(async_state)
        type(mcp_async_state_t), intent(inout) :: async_state

        integer :: exitcode

        if (async_state%active_pid > 0) then
            call process_cancel_pid(async_state%active_pid, exitcode)
            async_state%active_pid = 0
            async_state%last_run_id = async_state%active_run_id
            async_state%last_exitcode = 130
            async_state%last_output = async_state%active_output
        end if
        async_state%active_run_id = 0
        async_state%pending_run_id = 0
        async_state%active_output = ''
        async_state%queue%state = RUN_IDLE
        async_state%queue%rerun_pending = .false.
        async_state%queue%current_root = ''
        async_state%queue%current_mode = ''
        async_state%queue%pending_root = ''
        async_state%queue%pending_mode = ''
    end subroutine async_cancel_all

    subroutine requested_run_id(line, async_state, run_id, ierr)
        character(len=*), intent(in) :: line
        type(mcp_async_state_t), intent(in) :: async_state
        integer, intent(out) :: run_id, ierr

        character(len=64) :: run_text
        integer :: iostat

        ierr = 0
        run_id = async_state%last_run_id
        if (index(line, '"latest"') > 0 .or. index(line, '"run_id"') == 0) then
            if (async_state%active_run_id > 0) run_id = async_state%active_run_id
            if (async_state%pending_run_id > 0) run_id = async_state%pending_run_id
            return
        end if

        call extract_json_field(line, '"run_id"', run_text)
        read (run_text, *, iostat=iostat) run_id
        if (iostat /= 0) then
            ierr = 1
            return
        end if
        if (run_id == async_state%active_run_id) return
        if (run_id == async_state%pending_run_id) return
        if (run_id == async_state%last_run_id) return
        ierr = 1
    end subroutine requested_run_id

    subroutine make_initialize_response(id_str, line, response)
        character(len=*), intent(in) :: id_str, line
        character(len=*), intent(out) :: response

        character(len=32) :: proto_ver

        call extract_json_field(line, '"protocolVersion"', proto_ver)
        if (len_trim(proto_ver) == 0) proto_ver = '2025-03-26'
        response = '{"jsonrpc":"2.0","id":'//trim(id_str)//','// &
            '"result":{"protocolVersion":"'//trim(proto_ver)//'",'// &
            '"capabilities":{"tools":{"listChanged":false},'// &
            '"resources":{"listChanged":false}},'// &
            '"serverInfo":{"name":"fo","version":"0.2.0"}}}'
    end subroutine make_initialize_response

    subroutine make_tools_list_response(id_str, response)
        character(len=*), intent(in) :: id_str
        character(len=*), intent(out) :: response

        response = '{"jsonrpc":"2.0","id":'//trim(id_str)//','// &
            '"result":{"tools":[{"name":"fo",'// &
            '"description":"Fortran build driver",'// &
            '"inputSchema":{"type":"object","properties":{'// &
            '"action":{"type":"string",'// &
            '"enum":["check","status","diagnostics","cancel",'// &
            '"build","test","graph","info","changed","clean",'// &
            '"lint","fmt"],'// &
            '"description":"Action to run"},'// &
            '"dir":{"type":"string",'// &
            '"description":"Project directory (default: cwd)"},'// &
            '"fix":{"type":"boolean",'// &
            '"description":"lint: remove unused imports in place"}},'// &
            '"required":["action"]}}]}}'
    end subroutine make_tools_list_response

    subroutine make_resources_list_response(id_str, response)
        character(len=*), intent(in) :: id_str
        character(len=*), intent(out) :: response

        response = '{"jsonrpc":"2.0","id":'//trim(id_str)//','// &
            '"result":{"resources":[{"uri":"fo://diagnostics",'// &
            '"name":"diagnostics",'// &
            '"description":"Current fo check diagnostics",'// &
            '"mimeType":"text/plain"}]}}'
    end subroutine make_resources_list_response

    subroutine make_run_start_response(id_str, run_id, pending, response)
        character(len=*), intent(in) :: id_str
        integer, intent(in) :: run_id
        logical, intent(in) :: pending
        character(len=*), intent(out) :: response

        response = '{"jsonrpc":"2.0","id":'//trim(id_str)//','// &
            '"result":{"run_id":'//trim(json_int(run_id))// &
            ',"state":"'
        if (pending) then
            response = trim(response)//'rerun-pending"'
        else
            response = trim(response)//'running"'
        end if
        response = trim(response)//',"pending":'//trim(json_bool(pending))//'}}'
    end subroutine make_run_start_response

    subroutine make_tool_text_response(id_str, output_text, exitcode, response)
        character(len=*), intent(in) :: id_str, output_text
        integer, intent(in) :: exitcode
        character(len=*), intent(out) :: response

        character(len=16384) :: escaped

        escaped = json_escape_string(output_text)
        response = '{"jsonrpc":"2.0","id":'//trim(id_str)//','// &
            '"result":{"content":[{"type":"text",'// &
            '"text":"'//trim(escaped)//'"}],"isError":'// &
            trim(json_bool(exitcode /= 0))//'}}'
    end subroutine make_tool_text_response

end module fo_mcp
