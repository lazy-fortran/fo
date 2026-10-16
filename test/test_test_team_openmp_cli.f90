program test_test_team_openmp_cli
    !! Tests run as a concurrent team must not each start one OpenMP thread per
    !! core: the team multiplies the thread count and the CPU-time budget of
    !! every test is spent several times over. A team gives each test its share
    !! of the cores; a single test and any value the user
    !! exported are left alone.
    use fo_test_harness, only: process_result_t, string_list_t, list_add, &
        make_scratch, write_text, run_process, assert_process_ok, finish_assertions
    use fo_test_cli, only: resolve_driver
    implicit none
    character(len=*), parameter :: nl = new_line('a')
    character(:), allocatable :: driver, scratch

    call resolve_driver(driver)
    call make_scratch('fo-test-team-openmp', scratch)
    call write_text(scratch//'/fpm.toml', 'name="team_openmp"'//nl)
    call write_probe('test_probe_a')
    call write_probe('test_probe_b')

    ! FO_JOBS=8 -> a team of 2 tests, 8/2 = 4 threads each.
    call run_case('team default', [character(len=12) :: 'test_probe_a', 'test_probe_b'], &
        [character(len=20) :: 'OMP_EXPECT=4'])
    call run_case('user value wins', [character(len=12) :: 'test_probe_a', 'test_probe_b'], &
        [character(len=20) :: 'OMP_NUM_THREADS=3', 'OMP_EXPECT=3'])
    ! One test is not a team: nothing is imposed.
    call run_case('single test untouched', [character(len=12) :: 'test_probe_a'], &
        [character(len=20) :: 'OMP_EXPECT=none'])
    call finish_assertions()
contains
    subroutine write_probe(name)
        character(len=*), intent(in) :: name

        call write_text(scratch//'/test/'//name//'.f90', &
            'program '//name//nl// &
            'implicit none'//nl// &
            'character(len=64) :: got, want'//nl// &
            'integer :: st'//nl// &
            'call get_environment_variable("OMP_EXPECT", want)'//nl// &
            'call get_environment_variable("OMP_NUM_THREADS", got, status=st)'//nl// &
            'if (trim(want) == "none") then'//nl// &
            '    if (st == 0) error stop "OMP_NUM_THREADS was imposed"'//nl// &
            'else'//nl// &
            '    if (st /= 0) error stop "OMP_NUM_THREADS missing"'//nl// &
            '    if (trim(got) /= trim(want)) error stop "wrong OMP_NUM_THREADS"'//nl// &
            'end if'//nl// &
            'end program '//name//nl)
    end subroutine write_probe

    subroutine run_case(label, targets, extra)
        character(len=*), intent(in) :: label, targets(:), extra(:)
        type(string_list_t) :: command, env
        type(process_result_t) :: observed
        integer :: i

        call list_add(env, 'FO_BACKEND=fpm')
        call list_add(env, 'FO_JOBS=8')
        call list_add(env, 'FO_DISABLE_SELF_REFRESH=1')
        call list_add(env, 'FO_SELF_REFRESH=0')
        call list_add(env, 'FO_CACHE_DIR='//scratch//'/cache-'//label(1:4))
        do i = 1, size(extra)
            call list_add(env, trim(extra(i)))
        end do
        ! This test may itself run inside a team that exported OMP_NUM_THREADS,
        ! and the child inherits that; unless the case sets it, remove it.
        call list_add(command, 'env')
        do i = 1, size(extra)
            if (index(extra(i), 'OMP_NUM_THREADS=') == 1) exit
        end do
        if (i > size(extra)) then
            call list_add(command, '-u')
            call list_add(command, 'OMP_NUM_THREADS')
        end if
        call list_add(command, driver)
        call list_add(command, 'test')
        do i = 1, size(targets)
            call list_add(command, trim(targets(i)))
        end do
        call run_process(command, scratch, observed, env)
        call assert_process_ok(observed, label//': probes see the expected OpenMP environment')
    end subroutine run_case
end program test_test_team_openmp_cli
