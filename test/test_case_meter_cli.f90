program test_case_meter_cli
    !! Cost of each test case in the JSON report (#364): a case that touches
    !! 300 MB and writes 8 MB to a file reports at least that peak resident
    !! memory and, on Linux outside tmpfs, those bytes written, also when it
    !! writes them just before it ends, between two samples of the meter; a
    !! trivial case reports far less memory. Under a 100 MB limit from [extra.fo]
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
        'call sleep(1)'//nl// &
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
    call check_admission()
    call remove_tree(scratch)
    call finish_assertions()

contains

    subroutine check_admission()
        !! Two cases with a recorded peak of about 300 MB each hold it for a
        !! second: under a 400 MB budget they run one after the other, under
        !! 2 GB side by side (FO_JOBS=2 both times).
        character(:), allocatable :: dir
        type(string_list_t) :: run_env, run_args
        integer :: k

        dir = join_path(scratch, 'admit')
        call write_text(join_path(dir, 'fpm.toml'), 'name = "admit"'//nl)
        call write_text(join_path(dir, 'src/admit.f90'), &
            'module admit'//nl//'implicit none'//nl//'end module'//nl)
        do k = 1, 2
            call write_text(join_path(dir, 'test/hold_'//achar(96 + k)//'.f90'), &
                'program hold'//nl//'implicit none'//nl// &
                'real(8), allocatable :: a(:)'//nl// &
                'integer(8) :: t0, t1, rate'//nl//'integer :: u'//nl// &
                'allocate (a(37500000))'//nl//'a = 1.0d0'//nl// &
                'call system_clock(t0, rate)'//nl// &
                'open (newunit=u, file="'//join_path(dir, 'span_'//achar(96 + k))// &
                '", status="replace")'//nl// &
                'write (u, *) epoch_ms()'//nl// &
                'do'//nl//'call system_clock(t1)'//nl// &
                'if (t1 - t0 >= rate) exit'//nl//'end do'//nl// &
                'write (u, *) epoch_ms()'//nl//'close (u)'//nl// &
                'if (a(1) /= 1.0d0) error stop 1'//nl// &
                'contains'//nl// &
                'integer(8) function epoch_ms()'//nl// &
                'integer :: v(8)'//nl//'call date_and_time(values=v)'//nl// &
                'epoch_ms = ((int(v(3), 8)*24 + v(5))*60 + v(6))*60000 + v(7)*1000 + v(8)'//nl// &
                'end function'//nl//'end program'//nl)
        end do
        call list_add(run_env, 'FO_JOBS=2')
        call list_add(run_env, 'FO_TEST_RERUN=1')
        call list_add(run_args, 'test')
        call run_fo(driver, run_args, dir, join_path(scratch, 'cache'), result, &
            run_env, 240000)
        call assert_equal_integer(result%exit_code, 0, 'the holding cases pass: '// &
            result%stdout//result%stderr)

        call list_add(run_env, 'FO_TEST_MEMORY_BUDGET=400M')
        call run_fo(driver, run_args, dir, join_path(scratch, 'cache'), result, &
            run_env, 240000)
        call assert_equal_integer(result%exit_code, 0, 'admitted run passes: '// &
            result%stdout//result%stderr)
        call assert_true(.not. overlap(dir), &
            'cases whose recorded peaks exceed the budget together run apart')

        call list_add(run_env, 'FO_TEST_MEMORY_BUDGET=2G')
        call run_fo(driver, run_args, dir, join_path(scratch, 'cache'), result, &
            run_env, 240000)
        call assert_true(overlap(dir), 'cases that fit the budget run side by side')
    end subroutine check_admission

    logical function overlap(dir)
        character(len=*), intent(in) :: dir
        integer(8) :: a0, a1, b0, b1
        integer :: u, ios

        overlap = .false.
        open (newunit=u, file=join_path(dir, 'span_a'), status='old', iostat=ios)
        if (ios /= 0) return
        read (u, *) a0
        read (u, *) a1
        close (u)
        open (newunit=u, file=join_path(dir, 'span_b'), status='old', iostat=ios)
        if (ios /= 0) return
        read (u, *) b0
        read (u, *) b1
        close (u)
        overlap = a0 < b1 .and. b0 < a1
    end function overlap

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
