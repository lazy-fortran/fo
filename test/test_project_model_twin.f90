program test_project_model_twin
    !! Oracle for the backend-neutral project model: a twin fpm and CMake
    !! fixture (same sources, labels, layers, test groups, default exclusion)
    !! must give identical `fo test --list --json` selections, widening and
    !! diagnostics for a fixed request matrix, on an unbuilt and on a built
    !! tree. Plans are compared as sets of sources: the test-program sources
    !! must be identical, the CMake library sources must be exactly the link
    !! closure of the selected cases, and fpm (one library, always built
    !! whole) must cover them. Selection order is backend independent.
    !! Affected selection (`--only-changed` against Git HEAD): editing
    !! a core source, a phys source, a test source, a declared input, an
    !! unrelated file or the build description selects the same cases on
    !! both backends, and the expected ones; `fo test --only-changed` runs
    !! exactly them. Gremlin lanes started on both twins with the same
    !! request (all cases, named cases, an explicit slow case, an unknown
    !! case, a failing case) run the same cases with the same verdicts,
    !! labels and layers. `fo graph --layers --json` gives the same verdict
    !! and the same violation set (compared as a set) on the clean twin and on
    !! a twin with a broken layering, also for a project reached through a
    !! symlinked root and a symlinked source directory, whose paths stay
    !! project-relative. Runs the driver named by FO.
    use fo_test_harness, only: process_result_t, string_list_t, list_add
    use fo_test_harness, only: join_path, remove_tree, append_text
    use fo_test_harness, only: write_lines, assert_contains, assert_not_contains
    use fo_test_harness, only: assert_equal_integer, assert_equal_string
    use fo_test_harness, only: assert_true, finish_assertions
    use fo_test_cli, only: run_fo, run_external
    use fo_test_json, only: json_value_t, json_parse, json_member, json_element
    use fo_test_json, only: json_size, json_string_value, json_boolean_value
    use fo_test_json, only: json_number_value
    use fo_test_gremlin_oracle, only: gremlin_setup, gremlin_json, gremlin_wait_ms
    use fo_test_gremlin_oracle, only: gremlin_stop_lane, gremlin_process_running
    use fo_test_twin, only: twin_write
    use fo_test_mcp, only: mcp_encode, mcp_request, mcp_call, mcp_quote, mcp_exchange
    implicit none

    integer, parameter :: n_requests = 22
    character(len=40), parameter :: requests(n_requests) = [character(len=40) :: &
        '', '--all', 'test_core_a', 'test_phys_b test_io_c', 'test_*_b', &
        'test_?o*_*', 'test_slow_d --all', 'test_zz*', '-L unit', '-L io', &
        '-L slow', '-L unit -L io', '-LE unit', '-LE slow --all', &
        '--all -LE unit -LE io', '-L ^unit$ -LE io', '--layer tw_core', &
        '--layer tw_phys', '--layer tw_core --layer tw_phys', &
        '-L unit --layer tw_phys', '-L slow --layer tw_core', 'test_*_a -L io']
    integer, parameter :: n_errors = 5
    character(len=40), parameter :: errors(n_errors) = [character(len=40) :: &
        'test_slow_d', 'no_such_test', '--layer no_such_layer', &
        'test_core_a no_such_test', 'test_phys_b -L slow --layer tw_core']
    character(:), allocatable :: driver, scratch, fpm_dir, cmake_dir, cache
    character(:), allocatable :: project, state
    type(string_list_t) :: cmake_env, fpm_env
    integer :: pass

    call gremlin_setup(driver, scratch, project, cache, state)
    fpm_dir = join_path(scratch, 'fpm')
    cmake_dir = join_path(scratch, 'cmake')
    call list_add(cmake_env, 'FO_BACKEND=cmake')
    call list_add(cmake_env, 'FO_JOBS=2')
    call list_add(fpm_env, 'FO_JOBS=2')
    call twin_write(fpm_dir, 'fpm')
    call twin_write(cmake_dir, 'cmake')

    do pass = 1, 2
        call compare_matrix(merge('unbuilt', 'built  ', pass == 1))
        if (pass == 1) call build_both()
    end do
    call layering_matrix()
    call gremlin_matrix()
    call affected_matrix()

    call remove_tree(scratch)
    call finish_assertions()
    write (*, '(a)') 'project-model-twin: fpm and CMake selections and plans agree'

contains

    subroutine layering_matrix()
        !! `fo graph --layers --json`: same verdict on the clean twins, on
        !! twins with a broken layering and on the broken twins reached through
        !! a symlinked root with a symlinked source directory.
        character(:), allocatable :: viol_fpm, viol_cmake, link_fpm, link_cmake
        character(:), allocatable :: fpm_record, cmake_record, expected

        call compare_layering('clean', fpm_dir, cmake_dir, 0, fpm_record, cmake_record)
        call assert_equal_string(fpm_record, 'ok=T checked=2 layers=tw_core:;'// &
            'tw_phys:tw_core,; violations= unlayered=test/test_core_a.f90;'// &
            'test/test_io_c.f90;test/test_phys_b.f90;test/test_slow_d.f90; '// &
            'unscanned=', 'clean twin layering')
        viol_fpm = join_path(scratch, 'viol_fpm')
        viol_cmake = join_path(scratch, 'viol_cmake')
        call twin_write(viol_fpm, 'fpm', violation=.true.)
        call twin_write(viol_cmake, 'cmake', violation=.true.)
        call compare_layering('broken', viol_fpm, viol_cmake, 1, fpm_record, &
            cmake_record)
        expected = 'src/tw_aux.f90|tw_core|tw_aux|src/tw_core.f90|tw_core|'// &
            'tw_aux -> tw_core;src/tw_aux.f90|tw_phys|tw_aux|src/tw_phys.f90|'// &
            'tw_phys|tw_aux -> tw_phys;src/tw_phys.f90|tw_core|tw_phys|'// &
            'src/tw_core.f90|tw_core|tw_phys -> tw_core;'
        call assert_true(index(fpm_record, 'violations='//expected) > 0, &
            'broken twin reports the three violations')
        ! A symlinked source directory under a symlinked project root.
        call symlink_project(viol_fpm, 'fpm', link_fpm)
        call symlink_project(viol_cmake, 'cmake', link_cmake)
        call compare_layering('symlinked', link_fpm, link_cmake, 1, fpm_record, &
            cmake_record)
        call assert_true(index(fpm_record, 'violations='//expected) > 0, &
            'symlinked twin reports project-relative violation paths')
        call assert_true(index(fpm_record, scratch) == 0 .and. &
            index(cmake_record, scratch) == 0, 'no path leaves the project')
        call assert_text_relative(link_fpm, fpm_env, 'fpm')
        call assert_text_relative(link_cmake, cmake_env, 'cmake')
    end subroutine layering_matrix

    subroutine symlink_project(dir, backend, link)
        !! A copy of dir whose src is a symlink to a directory outside it,
        !! reached through a symlink to the project root.
        character(len=*), intent(in) :: dir, backend
        character(:), allocatable, intent(out) :: link
        character(:), allocatable :: copy, outside
        type(process_result_t) :: result

        copy = join_path(scratch, 'sym_'//backend)
        outside = join_path(scratch, 'outside_'//backend)
        link = join_path(scratch, 'root_'//backend)
        call run_external('cp', split_words('-r '//dir//' '//copy), scratch, result)
        call assert_equal_integer(result%exit_code, 0, 'copies the twin: '//result%stderr)
        call run_external('rm', split_words('-rf '//join_path(copy, 'build')), scratch, &
            result)
        call run_external('mv', split_words(join_path(copy, 'src')//' '//outside), &
            scratch, result)
        call assert_equal_integer(result%exit_code, 0, 'moves src out: '//result%stderr)
        call run_external('ln', split_words('-s '//outside//' '//join_path(copy, 'src')), &
            scratch, result)
        call assert_equal_integer(result%exit_code, 0, 'links src: '//result%stderr)
        call run_external('ln', split_words('-s '//copy//' '//link), scratch, result)
        call assert_equal_integer(result%exit_code, 0, 'links the root: '//result%stderr)
    end subroutine symlink_project

    subroutine assert_text_relative(dir, env, tag)
        !! The text report names project-relative paths only.
        character(len=*), intent(in) :: dir, tag
        type(string_list_t), intent(in) :: env
        type(process_result_t) :: result

        call run_fo(driver, split_words('graph --layers'), dir, cache, result, env)
        call assert_equal_integer(result%exit_code, 1, tag//' text layering exits 1')
        call assert_contains(result%stdout, 'violation: src/tw_phys.f90 (layer '// &
            'tw_phys) uses module tw_core from src/tw_core.f90', &
            tag//' text paths are project-relative')
        call assert_not_contains(result%stdout, scratch, &
            tag//' text report has no absolute path')
    end subroutine assert_text_relative

    subroutine compare_layering(tag, fpm_path, cmake_path, code, fpm_record, &
            cmake_record)
        character(len=*), intent(in) :: tag, fpm_path, cmake_path
        integer, intent(in) :: code
        character(:), allocatable, intent(out) :: fpm_record, cmake_record
        type(process_result_t) :: fpm_result, cmake_result
        type(json_value_t) :: fpm_report, cmake_report
        character(:), allocatable :: label

        label = 'layering ['//tag//'] '
        call run_fo(driver, split_words('graph --layers --json'), fpm_path, cache, &
            fpm_result, fpm_env)
        call run_fo(driver, split_words('graph --layers --json'), cmake_path, cache, &
            cmake_result, cmake_env)
        fpm_record = ''
        cmake_record = ''
        call assert_equal_integer(fpm_result%exit_code, code, label//'fpm exit: '// &
            fpm_result%stderr)
        call assert_equal_integer(cmake_result%exit_code, code, label//'cmake exit: '// &
            cmake_result%stderr)
        if (.not. parsed(fpm_result%stdout, fpm_report, label//'fpm')) return
        if (.not. parsed(cmake_result%stdout, cmake_report, label//'cmake')) return
        call assert_equal_string(json_string_value(json_member(fpm_report, 'backend')), &
            'fpm', label//'fpm backend')
        call assert_equal_string(json_string_value(json_member(cmake_report, &
            'backend')), 'cmake', label//'cmake backend')
        fpm_record = layering_record(fpm_report)
        cmake_record = layering_record(cmake_report)
        call assert_equal_string(cmake_record, fpm_record, label//'verdict and violations')
    end subroutine compare_layering

    function layering_record(report) result(text)
        !! Verdict, checked count, layers, the violation set, the unlayered
        !! and unscanned sources: everything but the backend name, with the
        !! lists sorted (violation order is each backend's own).
        type(json_value_t), intent(in) :: report
        character(:), allocatable :: text
        type(json_value_t) :: layers, layer, uses, violations, v
        character(len=300), allocatable :: items(:), layer_items(:)
        integer :: i, j

        text = 'ok='//merge('T', 'F', json_boolean_value(json_member(report, 'ok')))// &
            ' checked='
        text = text//number_text(json_number_value(json_member(report, 'checked')))// &
            ' layers='
        layers = json_member(report, 'layers')
        allocate (layer_items(json_size(layers)))
        do i = 1, json_size(layers)
            layer = json_element(layers, i)
            layer_items(i) = json_string_value(json_member(layer, 'name'))//':'
            uses = json_member(layer, 'uses')
            do j = 1, json_size(uses)
                layer_items(i) = trim(layer_items(i))// &
                    json_string_value(json_element(uses, j))//','
            end do
        end do
        text = text//sorted_join(layer_items)
        violations = json_member(report, 'violations')
        allocate (items(json_size(violations)))
        do i = 1, size(items)
            v = json_element(violations, i)
            items(i) = json_string_value(json_member(v, 'source'))//'|'// &
                json_string_value(json_member(v, 'module'))//'|'// &
                json_string_value(json_member(v, 'layer'))//'|'// &
                json_string_value(json_member(v, 'provider'))//'|'// &
                json_string_value(json_member(v, 'provider_layer'))//'|'// &
                json_string_value(json_member(v, 'missing_edge'))
        end do
        text = text//' violations='//sorted_join(items)
        text = text//' unlayered='//sorted_strings(json_member(report, 'unlayered'))// &
            ' unscanned='//sorted_strings(json_member(report, 'unscanned'))
    end function layering_record

    function sorted_strings(list) result(text)
        type(json_value_t), intent(in) :: list
        character(:), allocatable :: text
        character(len=300), allocatable :: items(:)
        integer :: i

        allocate (items(json_size(list)))
        do i = 1, size(items)
            items(i) = json_string_value(json_element(list, i))
        end do
        text = sorted_join(items)
    end function sorted_strings

    function number_text(value) result(text)
        use, intrinsic :: iso_fortran_env, only: real64
        real(real64), intent(in) :: value
        character(:), allocatable :: text
        character(len=24) :: buffer

        write (buffer, '(i0)') nint(value)
        text = trim(buffer)
    end function number_text

    subroutine gremlin_matrix()
        !! The same Gremlin request on both twins: same cases, verdicts,
        !! labels and layers (receipt sets), or no run on both.
        character(:), allocatable :: fail_fpm, fail_cmake
        character(len=48) :: fail_lines(4)

        call gremlin_pair('all', fpm_dir, cmake_dir, '', 4, &
            'build:BUILD_PASS;test_core_a|PASS|unit,|tw_core;'// &
            'test_io_c|PASS|io,|;test_phys_b|PASS|unit,|tw_phys;'// &
            'test_slow_d|PASS|slow,|;')
        call gremlin_pair('named', fpm_dir, cmake_dir, &
            '--target test_core_a --target test_phys_b --random 0', 2, &
            'build:BUILD_PASS;test_core_a|PASS|unit,|tw_core;'// &
            'test_phys_b|PASS|unit,|tw_phys;')
        call gremlin_pair('scoped', fpm_dir, cmake_dir, &
            '-L unit -LE io --layer tw_phys', 1, &
            'build:BUILD_PASS;test_phys_b|PASS|unit,|tw_phys;')
        call gremlin_pair('slow', fpm_dir, cmake_dir, '--target test_slow_d --random 0', &
            1, 'build:BUILD_PASS;test_slow_d|PASS|slow,|;')
        call gremlin_pair('unknown', fpm_dir, cmake_dir, &
            '--target no_such_test --random 0', 0, '')
        fail_fpm = join_path(scratch, 'fail_fpm')
        fail_cmake = join_path(scratch, 'fail_cmake')
        call twin_write(fail_fpm, 'fpm')
        call twin_write(fail_cmake, 'cmake')
        fail_lines = [character(len=48) :: 'program test_io_c', 'implicit none', &
            'error stop "io_c fails"', 'end program']
        call write_lines(join_path(fail_fpm, 'test/test_io_c.f90'), fail_lines)
        call write_lines(join_path(fail_cmake, 'test/test_io_c.f90'), fail_lines)
        call gremlin_pair('failing', fail_fpm, fail_cmake, '-L io', 1, &
            'build:BUILD_PASS;test_io_c|FAIL|io,|;')
    end subroutine gremlin_matrix

    subroutine gremlin_pair(tag, fpm_path, cmake_path, request_text, expected, &
            expected_record)
        character(len=*), intent(in) :: tag, fpm_path, cmake_path, request_text
        character(len=*), intent(in) :: expected_record
        integer, intent(in) :: expected
        character(:), allocatable :: fpm_session, cmake_session
        character(:), allocatable :: fpm_record, cmake_record, label

        label = 'gremlin ['//tag//'] '
        call gremlin_start(fpm_path, 'twin-'//tag//'-fpm', request_text, fpm_session)
        call gremlin_start(cmake_path, 'twin-'//tag//'-cmake', request_text, &
            cmake_session)
        call gremlin_record(fpm_path, 'twin-'//tag//'-fpm', fpm_session, expected, &
            fpm_record)
        call gremlin_record(cmake_path, 'twin-'//tag//'-cmake', cmake_session, &
            expected, cmake_record)
        call gremlin_stop_lane(driver, fpm_path, cache, state, 'twin-'//tag//'-fpm', &
            fpm_session, allow_terminal_error=.true.)
        call gremlin_stop_lane(driver, cmake_path, cache, state, &
            'twin-'//tag//'-cmake', cmake_session, allow_terminal_error=.true.)
        if (expected > 0) then
            call assert_equal_string(cmake_record, fpm_record, label//'cases and verdicts')
            call assert_equal_string(fpm_record, expected_record, &
                label//'expected receipts')
        else
            call assert_equal_string(fpm_record, 'rejected;', label//'fpm rejects case')
            call assert_equal_string(cmake_record, 'rejected;', label//'cmake rejects case')
        end if
    end subroutine gremlin_pair

    subroutine gremlin_start(dir, lane, request_text, session)
        character(len=*), intent(in) :: dir, lane, request_text
        character(:), allocatable, intent(out) :: session
        type(string_list_t) :: arguments
        type(process_result_t) :: result
        type(json_value_t) :: reply
        type(string_list_t) :: words
        integer :: i

        call list_add(arguments, 'gremlin')
        call list_add(arguments, 'start')
        call list_add(arguments, '--dir')
        call list_add(arguments, dir)
        call list_add(arguments, '--lane')
        call list_add(arguments, lane)
        words = split_words(request_text)
        if (allocated(words%items)) then
            do i = 1, size(words%items)
                call list_add(arguments, words%items(i)%value)
            end do
        end if
        call list_add(arguments, '--campaign-seconds')
        call list_add(arguments, '60')
        call list_add(arguments, '--timeout-seconds')
        call list_add(arguments, '20')
        call gremlin_json(driver, dir, cache, state, arguments, reply, result, 60000)
        session = json_string_value(json_member(reply, 'session_id'))
        call assert_true(result%exit_code == 0 .and. len(session) > 0, &
            lane//' starts: '//result%stderr)
    end subroutine gremlin_start

    subroutine gremlin_record(dir, lane, session, expected, record)
        !! Receipts of the lane once `expected` case receipts exist (a build
        !! failure ends the wait), or, for expected == 0, once the owner has
        !! exited: the sorted case|status|labels|layer set behind the build
        !! verdict, or 'no session;' when the owner never published one.
        character(len=*), intent(in) :: dir, lane, session
        integer, intent(in) :: expected
        character(:), allocatable, intent(out) :: record
        type(string_list_t) :: arguments
        type(process_result_t) :: result
        type(json_value_t) :: status, events, receipt, labels
        character(len=200), allocatable :: items(:)
        character(:), allocatable :: build, case_id, line
        integer :: poll, k, j, n_cases, dash, pid, ios
        character(:), allocatable :: owner_state
        logical :: done, has_session

        call list_add(arguments, 'gremlin')
        call list_add(arguments, 'status')
        call list_add(arguments, '--detail')
        call list_add(arguments, 'full')
        call list_add(arguments, '--dir')
        call list_add(arguments, dir)
        call list_add(arguments, '--lane')
        call list_add(arguments, lane)
        call list_add(arguments, '--session')
        call list_add(arguments, session)
        call list_add(arguments, '--max-records')
        call list_add(arguments, '100')
        dash = index(session, '-')
        pid = 0
        if (dash > 1) read (session(:dash - 1), *, iostat=ios) pid
        record = ''
        do poll = 1, 300
            call gremlin_json(driver, dir, cache, state, arguments, status, result, &
                20000)
            has_session = .not. json_has(status, 'error')
            events = json_member(status, 'events')
            build = ''
            n_cases = 0
            if (has_session) then
                allocate (items(max(1, json_size(events))))
                do k = 1, json_size(events)
                    receipt = json_element(events, k)
                    case_id = json_string_value(json_member(receipt, 'case_id'))
                    if (case_id == '<build>') then
                        build = json_string_value(json_member(receipt, 'status'))
                        cycle
                    end if
                    n_cases = n_cases + 1
                    labels = json_member(receipt, 'labels')
                    line = case_id//'|'//json_string_value(json_member(receipt, &
                        'status'))//'|'
                    do j = 1, json_size(labels)
                        line = line//json_string_value(json_element(labels, j))//','
                    end do
                    items(n_cases) = line//'|'//json_string_value(json_member(receipt, &
                        'layer'))
                end do
            end if
            done = .false.
            if (expected > 0) then
                done = n_cases >= expected .or. build == 'BUILD_FAIL'
            else
                owner_state = json_string_value(json_member(status, 'state'))
                done = build == 'BUILD_FAIL' .or. owner_state == 'inventory_failed' &
                    .or. owner_state == 'error'
                if (.not. done .and. pid > 0) done = .not. gremlin_process_running(pid)
            end if
            if (done) then
                if (has_session) then
                    record = 'build:'//build//';'//sorted_join(items(:n_cases))
                else
                    record = 'no session;'
                end if
                if (expected == 0 .and. n_cases == 0) then
                    ! Rejection may happen at build selection (CMake) or at
                    ! inventory selection (fpm); both must reject, never run.
                    if (build == 'BUILD_FAIL' .or. owner_state == 'inventory_failed' &
                        .or. owner_state == 'error') record = 'rejected;'
                end if
                if (allocated(items)) deallocate (items)
                return
            end if
            if (allocated(items)) deallocate (items)
            call gremlin_wait_ms(200)
        end do
        record = 'timeout;'
        call assert_true(.false., lane//' reaches its expected receipts')
    end subroutine gremlin_record

    logical function json_has(value, key) result(has)
        type(json_value_t), intent(in) :: value
        character(len=*), intent(in) :: key
        type(json_value_t) :: member

        member = json_member(value, key)
        has = len(json_string_value(member)) > 0
    end function json_has

    subroutine build_both()
        type(process_result_t) :: result
        type(string_list_t) :: arguments

        call list_add(arguments, 'build')
        call run_fo(driver, arguments, fpm_dir, cache, result, fpm_env)
        call assert_equal_integer(result%exit_code, 0, 'fpm twin builds: '// &
            result%stderr)
        call run_fo(driver, arguments, cmake_dir, cache, result, cmake_env)
        call assert_equal_integer(result%exit_code, 0, 'cmake twin builds: '// &
            result%stderr)
    end subroutine build_both

    subroutine affected_matrix()
        call git_baseline(fpm_dir)
        call git_baseline(cmake_dir)
        call compare_affected('clean tree', '', '', '')
        call compare_affected('core source', 'src/tw_core.f90', '', &
            'test_core_a;test_phys_b;')
        call compare_affected('phys source', 'src/tw_phys.f90', '', 'test_phys_b;')
        call compare_affected('test source', 'test/test_io_c.f90', '', 'test_io_c;')
        call compare_affected('declared input', 'data/io_ref.dat', '', 'test_io_c;')
        call compare_affected('unrelated file', 'notes.md', '', '')
        call compare_affected('build description', 'fpm.toml', 'CMakeLists.txt', &
            'test_core_a;test_io_c;test_phys_b;')
        call run_affected(fpm_dir, fpm_env, 'fpm')
        call run_affected(cmake_dir, cmake_env, 'cmake')
        call gremlin_only_changed()
    end subroutine affected_matrix

    subroutine gremlin_only_changed()
        !! The first Gremlin generation of an only-changed lane owes exactly
        !! the cases the model delta affects (not every case, not the
        !! compile-cache misses), on fresh twins without campaign history.
        character(:), allocatable :: fpm_path, cmake_path

        fpm_path = join_path(scratch, 'delta_fpm')
        cmake_path = join_path(scratch, 'delta_cmake')
        call twin_write(fpm_path, 'fpm')
        call twin_write(cmake_path, 'cmake')
        call git_baseline(fpm_path)
        call git_baseline(cmake_path)
        call edit(fpm_path, 'src/tw_phys.f90')
        call edit(cmake_path, 'src/tw_phys.f90')
        call gremlin_pair('only-changed', fpm_path, cmake_path, &
            '--only-changed --random 0', 1, &
            'build:BUILD_PASS;test_phys_b|PASS|unit,|tw_phys;')
        call assert_owed('fpm', fpm_path)
        call assert_owed('cmake', cmake_path)
    end subroutine gremlin_only_changed

    subroutine assert_owed(tag, dir)
        !! The lane is quiescent after owing exactly one case.
        character(len=*), intent(in) :: tag, dir
        character(:), allocatable :: session, lane
        type(string_list_t) :: arguments
        type(process_result_t) :: result
        type(json_value_t) :: status
        integer :: poll

        lane = 'owed-'//tag
        call gremlin_start(dir, lane, '--only-changed --random 0', session)
        call list_add(arguments, 'gremlin')
        call list_add(arguments, 'status')
        call list_add(arguments, '--dir')
        call list_add(arguments, dir)
        call list_add(arguments, '--lane')
        call list_add(arguments, lane)
        call list_add(arguments, '--session')
        call list_add(arguments, session)
        do poll = 1, 300
            call gremlin_json(driver, dir, cache, state, arguments, status, result, &
                20000)
            if (json_string_value(json_member(status, 'state')) == 'quiescent') exit
            call gremlin_wait_ms(200)
        end do
        call assert_equal_string(json_string_value(json_member(status, 'state')), &
            'quiescent', tag//' only-changed lane settles')
        call assert_equal_integer(nint(json_number_value(json_member(status, &
            'gate_required'))), 1, tag//' only-changed lane owes only the affected case')
        call gremlin_stop_lane(driver, dir, cache, state, lane, session, &
            allow_terminal_error=.true.)
    end subroutine assert_owed

    subroutine git_baseline(dir)
        character(len=*), intent(in) :: dir
        character(len=8) :: ignore(1)

        ignore(1) = 'build/'
        call write_lines(join_path(dir, '.gitignore'), ignore)
        call git(dir, 'init --quiet')
        call git(dir, 'add -A')
        call git(dir, '-c user.name=Twin -c user.email=twin@example.invalid '// &
            '-c commit.gpgsign=false commit --quiet -m twin')
    end subroutine git_baseline

    subroutine git(dir, text)
        character(len=*), intent(in) :: dir, text
        type(process_result_t) :: result

        call run_external('git', split_words(text), dir, result)
        call assert_equal_integer(result%exit_code, 0, 'git '//text//': '// &
            result%stderr)
    end subroutine git

    subroutine edit(dir, path)
        character(len=*), intent(in) :: dir, path

        if (len(path) == 0) return
        if (index(path, '.f90') > 0) then
            call append_text(join_path(dir, path), '! edited'//new_line('a'))
        else
            call append_text(join_path(dir, path), '# edited'//new_line('a'))
        end if
    end subroutine edit

    subroutine revert(dir)
        character(len=*), intent(in) :: dir

        call git(dir, 'checkout --quiet -- .')
        call git(dir, 'clean -f -q')
    end subroutine revert

    subroutine affected_request(dir, env, result)
        character(len=*), intent(in) :: dir
        type(string_list_t), intent(in) :: env
        type(process_result_t), intent(out) :: result

        call run_fo(driver, split_words('test --list --json --only-changed'), dir, &
            cache, result, env)
    end subroutine affected_request

    subroutine compare_affected(tag, fpm_path, cmake_path, expected)
        !! Edit, list the affected cases on both backends, compare, revert.
        character(len=*), intent(in) :: tag, fpm_path, cmake_path, expected
        type(process_result_t) :: fpm_result, cmake_result
        type(json_value_t) :: fpm_report, cmake_report
        character(:), allocatable :: label
        logical :: widened

        label = 'affected ['//tag//'] '
        widened = len(cmake_path) > 0
        call edit(fpm_dir, fpm_path)
        if (widened) then
            call edit(cmake_dir, cmake_path)
        else
            call edit(cmake_dir, fpm_path)
        end if
        call affected_request(fpm_dir, fpm_env, fpm_result)
        call affected_request(cmake_dir, cmake_env, cmake_result)
        call revert(fpm_dir)
        call revert(cmake_dir)
        call assert_equal_integer(fpm_result%exit_code, 0, label//'fpm lists: '// &
            fpm_result%stderr)
        call assert_equal_integer(cmake_result%exit_code, 0, label//'cmake lists: '// &
            cmake_result%stderr)
        if (.not. parsed(fpm_result%stdout, fpm_report, label//'fpm')) return
        if (.not. parsed(cmake_result%stdout, cmake_report, label//'cmake')) return
        call assert_equal_string(case_names(fpm_report), expected, &
            label//'fpm affected cases')
        call assert_equal_string(case_names(cmake_report), expected, &
            label//'cmake affected cases')
        if (widened) then
            ! The reasons name each backend's own build description.
            call assert_contains(fpm_result%stderr, 'affected selection widened', &
                label//'fpm reports the widening')
            call assert_contains(cmake_result%stderr, 'affected selection widened', &
                label//'cmake reports the widening')
        else
            call assert_equal_string(case_records(cmake_report), &
                case_records(fpm_report), label//'case records')
            call assert_not_contains(fpm_result%stderr, 'widened', &
                label//'fpm does not widen')
            call assert_not_contains(cmake_result%stderr, 'widened', &
                label//'cmake does not widen')
        end if
    end subroutine compare_affected

    subroutine run_affected(dir, env, tag)
        !! `fo test --only-changed` after a phys edit runs test_phys_b only.
        character(len=*), intent(in) :: dir, tag
        type(string_list_t), intent(in) :: env
        type(process_result_t) :: result

        type(json_value_t), allocatable :: responses(:)
        type(json_value_t) :: field
        character(:), allocatable :: messages, text

        call edit(dir, 'src/tw_phys.f90')
        call run_fo(driver, split_words('test --only-changed'), dir, cache, result, env)
        call assert_equal_integer(result%exit_code, 0, tag//' affected run: '// &
            result%stderr)
        call assert_contains(result%stdout//result%stderr, 'test_phys_b', &
            tag//' affected run runs test_phys_b')
        call assert_not_contains(result%stdout//result%stderr, 'test_core_a', &
            tag//' affected run skips test_core_a')
        ! MCP test with only_changed runs the same cases.
        messages = mcp_encode(mcp_request(1, 'initialize', &
            '{"protocolVersion":"2025-11-25","capabilities":{}}'), .false.)
        messages = messages//mcp_encode(mcp_call(4, '{"action":"changed","dir":'// &
            mcp_quote(dir)//'}'), .false.)
        messages = messages//mcp_encode(mcp_call(2, '{"action":"test","dir":'// &
            mcp_quote(dir)//',"only_changed":true,"json":"full"}'), .false.)
        messages = messages//mcp_encode(mcp_request(3, 'shutdown'), .false.)
        call mcp_exchange(driver, dir, cache, messages, .false., responses, result)
        call revert(dir)
        call assert_equal_integer(result%exit_code, 0, tag//' MCP exits cleanly')
        if (size(responses) < 3) then
            call assert_true(.false., tag//' MCP answers the test call')
            return
        end if
        field = json_member(json_member(responses(3), 'result'), 'isError')
        call assert_true(.not. json_boolean_value(field), &
            tag//' MCP affected run passes')
        field = json_member(json_element(json_member(json_member(responses(3), &
            'result'), 'content'), 1), 'text')
        text = json_string_value(field)
        call assert_contains(text, 'test_phys_b', tag//' MCP runs test_phys_b')
        call assert_not_contains(text, 'test_core_a', tag//' MCP skips test_core_a')
        ! The changed action lists model cases: a cold cache counts every
        ! source as changed, so it names the default selection, the same on
        ! both backends (no slow case, no per-program duplicates).
        field = json_member(json_element(json_member(json_member(responses(2), &
            'result'), 'content'), 1), 'text')
        text = json_string_value(field)
        call assert_contains(text, 'affected tests (3):'//new_line('a')// &
            '  test_core_a'//new_line('a')//'  test_io_c'//new_line('a')// &
            '  test_phys_b', tag//' MCP changed lists the affected model cases')
    end subroutine run_affected

    function case_names(report) result(text)
        type(json_value_t), intent(in) :: report
        character(:), allocatable :: text
        character(len=64), allocatable :: items(:)
        type(json_value_t) :: cases
        integer :: i

        cases = json_member(report, 'cases')
        allocate (items(json_size(cases)))
        do i = 1, json_size(cases)
            items(i) = json_string_value(json_member(json_element(cases, i), 'name'))
        end do
        text = sorted_join(items)
    end function case_names

    function split_words(text) result(list)
        character(len=*), intent(in) :: text
        type(string_list_t) :: list
        integer :: pos, next

        pos = 1
        do while (pos <= len_trim(text))
            next = index(text(pos:), ' ')
            if (next == 0) next = len_trim(text) - pos + 2
            if (next > 1) call list_add(list, text(pos:pos + next - 2))
            pos = pos + next
        end do
    end function split_words

    subroutine request(dir, env, text, result)
        character(len=*), intent(in) :: dir, text
        type(string_list_t), intent(in) :: env
        type(process_result_t), intent(out) :: result
        type(string_list_t) :: arguments
        integer :: pos, next

        call list_add(arguments, 'test')
        call list_add(arguments, '--list')
        call list_add(arguments, '--json')
        pos = 1
        do while (pos <= len_trim(text))
            next = index(text(pos:), ' ')
            if (next == 0) next = len_trim(text) - pos + 2
            if (next > 1) call list_add(arguments, text(pos:pos + next - 2))
            pos = pos + next
        end do
        call run_fo(driver, arguments, dir, cache, result, env)
    end subroutine request

    subroutine compare_matrix(tag)
        character(len=*), intent(in) :: tag
        integer :: i

        do i = 1, n_requests
            call compare_request(tag, trim(requests(i)))
        end do
        do i = 1, n_errors
            call compare_error(tag, trim(errors(i)))
        end do
    end subroutine compare_matrix

    subroutine compare_error(tag, text)
        character(len=*), intent(in) :: tag, text
        type(process_result_t) :: fpm_result, cmake_result
        character(:), allocatable :: label

        label = tag//' ['//text//'] '
        call request(fpm_dir, fpm_env, text, fpm_result)
        call request(cmake_dir, cmake_env, text, cmake_result)
        call assert_equal_integer(cmake_result%exit_code, fpm_result%exit_code, &
            label//'exit code')
        call assert_equal_string(first_line(cmake_result%stderr), &
            first_line(fpm_result%stderr), label//'diagnostic')
    end subroutine compare_error

    function first_line(text) result(line)
        character(len=*), intent(in) :: text
        character(:), allocatable :: line
        integer :: k

        k = index(text, new_line('a'))
        if (k == 0) k = len(text) + 1
        line = text(:k - 1)
    end function first_line

    subroutine compare_request(tag, text)
        character(len=*), intent(in) :: tag, text
        type(process_result_t) :: fpm_result, cmake_result
        type(json_value_t) :: fpm_report, cmake_report
        character(:), allocatable :: label, fpm_cases, cmake_cases
        character(:), allocatable :: fpm_lib, cmake_lib, fpm_tests, cmake_tests
        character(:), allocatable :: required

        label = tag//' ['//text//'] '
        call request(fpm_dir, fpm_env, text, fpm_result)
        call request(cmake_dir, cmake_env, text, cmake_result)
        call assert_equal_integer(fpm_result%exit_code, 0, label//'fpm lists: '// &
            fpm_result%stderr)
        call assert_equal_integer(cmake_result%exit_code, 0, label//'cmake lists: '// &
            cmake_result%stderr)
        if (.not. parsed(fpm_result%stdout, fpm_report, label//'fpm')) return
        if (.not. parsed(cmake_result%stdout, cmake_report, label//'cmake')) return

        fpm_cases = case_records(fpm_report)
        cmake_cases = case_records(cmake_report)
        call assert_equal_string(cmake_cases, fpm_cases, label//'selected cases')
        call assert_true(json_boolean_value(json_member(cmake_report, 'widened')) .eqv. &
            json_boolean_value(json_member(fpm_report, 'widened')), label//'widened')
        call assert_true(.not. json_boolean_value(json_member(cmake_report, &
            'widened')), label//'cmake plan does not widen')
        call assert_equal_integer(json_size(json_member(cmake_report, &
            'widen_reasons')), json_size(json_member(fpm_report, 'widen_reasons')), &
            label//'widen reasons')

        call plan_sources(fpm_report, .true., fpm_tests, fpm_lib)
        call plan_sources(cmake_report, .false., cmake_tests, cmake_lib)
        call assert_equal_string(cmake_tests, fpm_tests, label//'plan test sources')
        required = required_library_sources(cmake_report)
        call assert_equal_string(cmake_lib, required, &
            label//'cmake plan library sources are the link closure')
        call assert_true(covers(fpm_lib, required), &
            label//'fpm plan builds the library sources the cases need')
    end subroutine compare_request

    logical function parsed(text, report, label) result(valid)
        character(len=*), intent(in) :: text, label
        type(json_value_t), intent(out) :: report
        character(:), allocatable :: message

        call json_parse(text, report, valid, message)
        call assert_true(valid, label//' JSON parses')
    end function parsed

    function case_records(report) result(text)
        !! name|labels|layer|reason of every selected case, in list order.
        type(json_value_t), intent(in) :: report
        character(:), allocatable :: text
        character(len=200), allocatable :: items(:)
        type(json_value_t) :: cases, c, labels
        integer :: i, j
        character(:), allocatable :: line

        cases = json_member(report, 'cases')
        allocate (items(json_size(cases)))
        do i = 1, json_size(cases)
            c = json_element(cases, i)
            labels = json_member(c, 'labels')
            line = json_string_value(json_member(c, 'name'))//'|'
            do j = 1, json_size(labels)
                line = line//json_string_value(json_element(labels, j))//','
            end do
            items(i) = line//'|'//json_string_value(json_member(c, 'layer'))//'|'// &
                json_string_value(json_member(c, 'reason'))
        end do
        text = ''
        do i = 1, size(items)
            text = text//trim(items(i))//';'
        end do
    end function case_records

    subroutine plan_sources(report, is_fpm, tests, lib)
        !! Sources of the plan targets, split into test programs and
        !! library sources, from the fixture's known target contents.
        type(json_value_t), intent(in) :: report
        logical, intent(in) :: is_fpm
        character(:), allocatable, intent(out) :: tests, lib
        type(json_value_t) :: targets
        character(len=200), allocatable :: t(:), l(:)
        character(:), allocatable :: name
        integer :: i, nt, nl

        targets = json_member(report, 'targets')
        allocate (t(json_size(targets)), l(2*json_size(targets)))
        nt = 0
        nl = 0
        do i = 1, json_size(targets)
            name = json_string_value(json_element(targets, i))
            if (name(1:5) == 'test_') then
                nt = nt + 1
                t(nt) = 'test/'//name//'.f90'
            else if (name == 'libtw') then
                l(nl + 1) = 'src/tw_core.f90'
                l(nl + 2) = 'src/tw_phys.f90'
                nl = nl + 2
            else
                nl = nl + 1
                l(nl) = 'src/'//name//'.f90'
            end if
        end do
        call assert_true(is_fpm .or. name_free(targets, 'libtw'), &
            'cmake plan has no fpm library target')
        tests = sorted_join(t(1:nt))
        lib = sorted_join(l(1:nl))
    end subroutine plan_sources

    logical function name_free(targets, name) result(free)
        type(json_value_t), intent(in) :: targets
        character(len=*), intent(in) :: name
        integer :: i

        free = .true.
        do i = 1, json_size(targets)
            if (json_string_value(json_element(targets, i)) == name) free = .false.
        end do
    end function name_free

    function required_library_sources(report) result(text)
        !! Library sources in the link closure of the selected cases:
        !! core_a needs core, phys_b needs core and phys.
        type(json_value_t), intent(in) :: report
        character(:), allocatable :: text
        type(json_value_t) :: cases
        character(:), allocatable :: name
        logical :: core, phys
        integer :: i

        core = .false.
        phys = .false.
        cases = json_member(report, 'cases')
        do i = 1, json_size(cases)
            name = json_string_value(json_member(json_element(cases, i), 'name'))
            if (name == 'test_core_a') core = .true.
            if (name == 'test_phys_b') then
                core = .true.
                phys = .true.
            end if
        end do
        text = ''
        if (core) text = text//'src/tw_core.f90;'
        if (phys) text = text//'src/tw_phys.f90;'
    end function required_library_sources

    logical function covers(have, need) result(ok)
        !! Every ';'-terminated item of need occurs in have.
        character(len=*), intent(in) :: have, need
        integer :: pos, next

        ok = .true.
        pos = 1
        do while (pos <= len(need))
            next = index(need(pos:), ';')
            if (index(have, need(pos:pos + next - 1)) == 0) ok = .false.
            pos = pos + next
        end do
    end function covers

    function sorted_join(items) result(text)
        !! Items sorted and deduplicated, each followed by ';'.
        character(len=*), intent(in) :: items(:)
        character(:), allocatable :: text
        character(len=len(items)) :: held
        character(len=len(items)), allocatable :: work(:)
        integer :: i, j

        work = items
        do i = 2, size(work)
            held = work(i)
            j = i - 1
            do while (j >= 1)
                if (work(j) <= held) exit
                work(j + 1) = work(j)
                j = j - 1
            end do
            work(j + 1) = held
        end do
        text = ''
        do i = 1, size(work)
            if (i > 1) then
                if (work(i) == work(i - 1)) cycle
            end if
            text = text//trim(work(i))//';'
        end do
    end function sorted_join

end program test_project_model_twin
