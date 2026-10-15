program test_project_model_twin
    !! Oracle for the backend-neutral project model: a twin fpm and CMake
    !! fixture (same sources, labels, layers, test groups, default exclusion)
    !! must give identical `fo test --list --json` selections, widening and
    !! diagnostics for a fixed request matrix, on an unbuilt and on a built
    !! tree. Plans are compared as sets of sources: the test-program sources
    !! must be identical, the CMake library sources must be exactly the link
    !! closure of the selected cases, and fpm (one library, always built
    !! whole) must cover them. Case order is not compared: CTest orders by
    !! cost. Affected selection (`--only-changed` against Git HEAD): editing
    !! a core source, a phys source, a test source, a declared input, an
    !! unrelated file or the build description selects the same cases on
    !! both backends, and the expected ones; `fo test --only-changed` runs
    !! exactly them. Runs the driver named by FO.
    use fo_test_harness, only: process_result_t, string_list_t, list_add
    use fo_test_harness, only: make_scratch, join_path, remove_tree, append_text
    use fo_test_harness, only: write_lines, assert_contains, assert_not_contains
    use fo_test_harness, only: assert_equal_integer, assert_equal_string
    use fo_test_harness, only: assert_true, finish_assertions
    use fo_test_cli, only: resolve_driver, run_fo, run_external
    use fo_test_json, only: json_value_t, json_parse, json_member, json_element
    use fo_test_json, only: json_size, json_string_value, json_boolean_value
    use fo_test_twin, only: twin_write
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
    type(string_list_t) :: cmake_env, fpm_env
    integer :: pass

    call resolve_driver(driver)
    call make_scratch('fo-project-model-twin', scratch)
    cache = join_path(scratch, 'cache')
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
    call affected_matrix()

    call remove_tree(scratch)
    call finish_assertions()
    write (*, '(a)') 'project-model-twin: fpm and CMake selections and plans agree'

contains

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
    end subroutine affected_matrix

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

        call edit(dir, 'src/tw_phys.f90')
        call run_fo(driver, split_words('test --only-changed'), dir, cache, result, env)
        call revert(dir)
        call assert_equal_integer(result%exit_code, 0, tag//' affected run: '// &
            result%stderr)
        call assert_contains(result%stdout//result%stderr, 'test_phys_b', &
            tag//' affected run runs test_phys_b')
        call assert_not_contains(result%stdout//result%stderr, 'test_core_a', &
            tag//' affected run skips test_core_a')
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
