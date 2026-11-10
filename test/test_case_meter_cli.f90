program test_case_meter_cli
    !! Cost of each test case in the JSON report (#364): a case that touches
    !! 300 MB and writes 8 MB to a file reports at least that peak resident
    !! memory and, on Linux outside tmpfs, those bytes written; a trivial case
    !! reports far less memory. Under a 100 MB limit from [extra.fo]
    !! test-memory-limit the large case alone ends as LIMIT, and
    !! FO_TEST_MEMORY_LIMIT overrides the manifest. Runs the driver named by FO.
    use fo_test_harness, only: process_result_t, string_list_t, list_add
    use fo_test_harness, only: make_scratch, join_path, write_text, read_text
    use fo_test_harness, only: remove_tree, assert_equal_integer, assert_true
    use fo_test_harness, only: finish_assertions, assert_equal_string
    use fo_test_cli, only: resolve_driver, run_fo
    use fo_test_json, only: json_value_t, json_parse, json_member, json_element
    use fo_test_json, only: json_size, json_string_value, json_number_value
    implicit none

    character(len=1), parameter :: nl = achar(10)
    character(:), allocatable :: driver, scratch, project, message
    type(string_list_t) :: env, arguments, listing
    type(process_result_t) :: result
    type(json_value_t) :: report, tests, entry
    real(8) :: heavy_peak, light_peak, heavy_written, heavy_listed
    logical :: valid, linux
    integer :: i

    call resolve_driver(driver)
    call make_scratch('fo-case-meter', scratch)
    project = join_path(scratch, 'project')
    call write_text(join_path(project, 'fpm.toml'), 'name = "meter"'//nl)
    call write_text(join_path(project, 'src/meter.f90'), &
        'module meter'//nl//'implicit none'//nl//'end module'//nl)
    call write_text(join_path(project, 'test/meter_heavy.f90'), &
        'program meter_heavy'//nl// &
        'implicit none'//nl// &
        'real(8), allocatable :: a(:)'//nl// &
        'character(len=1048576) :: block'//nl// &
        'integer :: k, u'//nl// &
        'allocate (a(37500000))'//nl// &
        'a = 1.0d0'//nl// &
        'block = repeat("x", len(block))'//nl// &
        'open (newunit=u, file="meter_out.bin", access="stream", status="replace")'//nl// &
        'do k = 1, 8'//nl// &
        '    write (u) block'//nl// &
        'end do'//nl// &
        'close (u)'//nl// &
        'if (sum(a(1:10)) /= 10.0d0) error stop 1'//nl// &
        'end program'//nl)
    call write_text(join_path(project, 'test/meter_light.f90'), &
        'program meter_light'//nl//'implicit none'//nl//'end program'//nl)
    call list_add(env, 'FO_JOBS=1')
    call list_add(env, 'FO_TEST_RERUN=1')
    call list_add(arguments, 'test')
    call list_add(arguments, '--json=full')
    call run_fo(driver, arguments, project, join_path(scratch, 'cache'), result, &
        env, 240000)
    call assert_equal_integer(result%exit_code, 0, 'both cases pass: '// &
        result%stdout//result%stderr)
    call json_parse(result%stdout, report, valid, message)
    call assert_true(valid, 'the report is JSON: '//result%stdout)
    heavy_peak = -1
    light_peak = -1
    heavy_written = -1
    tests = json_member(report, 'tests')
    do i = 1, json_size(tests)
        entry = json_element(tests, i)
        select case (json_string_value(json_member(entry, 'name')))
        case ('meter_heavy')
            heavy_peak = json_number_value(json_member(entry, 'peak_rss_kb'))
            heavy_written = json_number_value(json_member(entry, 'write_bytes'))
        case ('meter_light')
            light_peak = json_number_value(json_member(entry, 'peak_rss_kb'))
        end select
    end do
    call assert_true(heavy_peak >= 280000.0d0, &
        'the 300 MB case reports its peak memory: '//result%stdout)
    call assert_true(light_peak >= 0.0d0 .and. light_peak < 100000.0d0, &
        'the trivial case reports a small peak: '//result%stdout)
    call list_add(listing, 'test')
    call list_add(listing, '--list')
    call list_add(listing, '--json')
    call run_fo(driver, listing, project, join_path(scratch, 'cache'), result, &
        env, 240000)
    call json_parse(result%stdout, report, valid, message)
    heavy_listed = -1
    tests = json_member(report, 'cases')
    do i = 1, json_size(tests)
        entry = json_element(tests, i)
        if (json_string_value(json_member(entry, 'name')) == 'meter_heavy') &
            heavy_listed = json_number_value(json_member(entry, 'peak_rss_kb'))
    end do
    call assert_true(heavy_listed >= 280000.0d0, &
        'the plan lists the recorded peak of the large case: '//result%stdout)
    inquire (file='/proc/self/io', exist=linux)
    if (linux .and. .not. on_tmpfs(project)) then
        call assert_true(heavy_written >= 8.0d6, &
            'the case that writes 8 MB reports those bytes: '//result%stdout)
    end if

    call write_text(join_path(project, 'fpm.toml'), 'name = "meter"'//nl// &
        '[extra.fo]'//nl//'test-memory-limit = "100M"'//nl)
    call run_fo(driver, arguments, project, join_path(scratch, 'cache'), result, &
        env, 240000)
    call assert_true(result%exit_code /= 0, 'a case over its memory limit fails')
    call assert_equal_string(status_of('meter_heavy'), 'limit', &
        'the case over the limit ends as LIMIT: '//result%stdout)
    call assert_equal_string(status_of('meter_light'), 'pass', &
        'the small case still passes: '//result%stdout)

    call list_add(env, 'FO_TEST_MEMORY_LIMIT=1G')
    call run_fo(driver, arguments, project, join_path(scratch, 'cache'), result, &
        env, 240000)
    call assert_equal_integer(result%exit_code, 0, &
        'FO_TEST_MEMORY_LIMIT overrides the manifest: '//result%stdout//result%stderr)
    call remove_tree(scratch)
    call finish_assertions()

contains

    function status_of(name) result(status)
        character(len=*), intent(in) :: name
        character(:), allocatable :: status, text
        type(json_value_t) :: parsed, cases, item
        logical :: ok
        integer :: k

        status = 'missing'
        call json_parse(result%stdout, parsed, ok, text)
        if (.not. ok) return
        cases = json_member(parsed, 'tests')
        do k = 1, json_size(cases)
            item = json_element(cases, k)
            if (json_string_value(json_member(item, 'name')) == name) &
                status = json_string_value(json_member(item, 'status'))
        end do
    end function status_of


    logical function on_tmpfs(path)
        !! The longest mount point of /proc/self/mounts that contains path
        !! has type tmpfs, which writes nothing to storage.
        character(len=*), intent(in) :: path
        character(:), allocatable :: text, line, point, kind
        integer :: at, eol, s1, s2, s3, best

        on_tmpfs = .false.
        text = read_text('/proc/self/mounts')
        best = 0
        at = 1
        do while (at <= len(text))
            eol = index(text(at:), nl)
            if (eol == 0) exit
            line = text(at:at + eol - 2)
            at = at + eol
            s1 = index(line, ' ')
            s2 = s1 + index(line(s1 + 1:), ' ')
            s3 = s2 + index(line(s2 + 1:), ' ')
            if (s1 == 0 .or. s2 <= s1 .or. s3 <= s2) cycle
            point = line(s1 + 1:s2 - 1)
            kind = line(s2 + 1:s3 - 1)
            if (index(path, point) /= 1 .or. len(point) <= best) cycle
            if (len(point) > 1 .and. len(path) > len(point)) then
                if (path(len(point) + 1:len(point) + 1) /= '/') cycle
            end if
            best = len(point)
            on_tmpfs = kind == 'tmpfs'
        end do
    end function on_tmpfs

end program test_case_meter_cli
