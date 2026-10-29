module fo_test_budget
    !! Per-test time limits. A test gets a budget of user CPU time, not a
    !! wall-clock one (system time is not charged: host load inflates it
    !! without bound, see child_cpu_ms in fo_process.c): on a loaded host a test that needs three seconds of CPU can take
    !! thirty of wall time, and whether it passes must not depend on what else
    !! the machine is doing. A much larger wall-clock cap still catches tests
    !! that hang without using CPU (blocked on I/O, sleeping, deadlocked).
    !!
    !! Precedence for each limit: environment variable, then fpm.toml
    !! [extra.fo], then the default.
    use fo_fpm_config, only: fpm_config_t
    use fo_pattern, only: pattern_glob
    use fo_process, only: TIMEOUT_CPU, TIMEOUT_WALL, TIMEOUT_UNMEASURED
    implicit none
    private

    public :: test_timeout_seconds, slow_test_timeout_seconds
    public :: test_budget_seconds, test_wall_cap_seconds
    public :: ctest_default_timeout_seconds
    public :: timeout_detail
    public :: test_policy, test_strict_flaky, test_retries_override
    public :: test_set_retry_options

    integer, parameter :: DEFAULT_TEST_TIMEOUT = 10
    integer, parameter :: DEFAULT_SLOW_TEST_TIMEOUT = 300
    integer, parameter :: WALL_CAP_FACTOR = 10
    !! ctest's limit for a test without TIMEOUT (DART_TESTING_TIMEOUT of the
    !! CTest module).
    integer, parameter, public :: CTEST_DEFAULT_TIMEOUT = 1500
    !! The isolated rerun fo always did for a failure: one retry.
    integer, parameter, public :: DEFAULT_TEST_RETRIES = 1
    !! `fo test --retries N` and `--strict-flaky` of this process.
    integer, save :: cli_retries = -1
    logical, save :: cli_strict_flaky = .false.

contains

    integer function test_timeout_seconds(config) result(t)
        !! CPU budget of a regular test (FO_TEST_TIMEOUT, test-timeout).
        type(fpm_config_t), intent(in), optional :: config

        t = DEFAULT_TEST_TIMEOUT
        if (present(config)) then
            if (config%test_timeout > 0) t = config%test_timeout
        end if
        call environment_seconds('FO_TEST_TIMEOUT', t)
    end function test_timeout_seconds

    integer function slow_test_timeout_seconds(config) result(t)
        !! CPU budget of a test named *_slow (FO_SLOW_TEST_TIMEOUT,
        !! slow-test-timeout). Naming a test slow only kept it out of the
        !! default run; it still needs room to finish under `fo test --all`.
        type(fpm_config_t), intent(in), optional :: config

        t = DEFAULT_SLOW_TEST_TIMEOUT
        if (present(config)) then
            if (config%slow_test_timeout > 0) t = config%slow_test_timeout
        end if
        call environment_seconds('FO_SLOW_TEST_TIMEOUT', t)
    end function slow_test_timeout_seconds

    integer function test_budget_seconds(config, slow) result(t)
        !! CPU budget of one test, by whether it is marked slow.
        type(fpm_config_t), intent(in), optional :: config
        logical, intent(in) :: slow

        if (slow) then
            t = slow_test_timeout_seconds(config)
        else
            t = test_timeout_seconds(config)
        end if
    end function test_budget_seconds

    integer function test_wall_cap_seconds(config, budget) result(t)
        !! Wall-clock safety cap for a test with the given CPU budget
        !! (FO_TEST_WALL_TIMEOUT, test-wall-timeout; default ten times the
        !! budget). Never below the budget; equal to it restores a plain
        !! wall-clock limit.
        type(fpm_config_t), intent(in), optional :: config
        integer, intent(in) :: budget

        t = 0
        if (present(config)) t = config%test_wall_timeout
        call environment_seconds('FO_TEST_WALL_TIMEOUT', t)
        if (t <= 0) t = WALL_CAP_FACTOR * budget
        t = max(t, budget)
    end function test_wall_cap_seconds

    integer function ctest_default_timeout_seconds() result(t)
        !! Wall limit of a CTest case without its own TIMEOUT property:
        !! FO_TEST_WALL_TIMEOUT, else the wall cap of an explicit
        !! FO_TEST_TIMEOUT, else ctest's default. The fpm budget does not
        !! apply because a CMake project never opted into it.
        integer :: budget

        t = 0
        call environment_seconds('FO_TEST_WALL_TIMEOUT', t)
        if (t > 0) return
        budget = 0
        call environment_seconds('FO_TEST_TIMEOUT', budget)
        t = CTEST_DEFAULT_TIMEOUT
        if (budget > 0) t = WALL_CAP_FACTOR*budget
    end function ctest_default_timeout_seconds

    subroutine test_policy(config, name, source, retries, slow_period, slow_after)
        !! Retries after a failure and the SLOW period and terminate-after
        !! count of one test (#217). Retries: FO_TEST_RETRIES (`--retries`),
        !! the first matching [extra.fo.test-groups] entry that sets them,
        !! [extra.fo] test-retries, then one retry. The SLOW bounds: the
        !! first matching group entry that sets slow-timeout, then [extra.fo]
        !! slow-timeout; period 0 means no SLOW bound.
        type(fpm_config_t), intent(in) :: config
        character(len=*), intent(in) :: name, source
        integer, intent(out) :: retries, slow_period, slow_after
        integer :: g, k
        logical :: hit
        integer :: value
        logical :: retries_set, slow_set

        retries = DEFAULT_TEST_RETRIES
        if (config%test_retries >= 0) retries = config%test_retries
        slow_period = config%slow_period
        slow_after = config%slow_after
        retries_set = .false.
        slow_set = .false.
        do g = 1, config%n_test_groups
            hit = .false.
            do k = 1, size(config%test_groups(g)%tests)
                if (pattern_glob(config%test_groups(g)%tests(k)%s, name) .or. &
                    pattern_glob(config%test_groups(g)%tests(k)%s, source)) then
                    hit = .true.
                    exit
                end if
            end do
            if (.not. hit) cycle
            if (config%test_groups(g)%retries >= 0 .and. .not. retries_set) then
                retries = config%test_groups(g)%retries
                retries_set = .true.
            end if
            if (config%test_groups(g)%slow_period > 0 .and. .not. slow_set) then
                slow_period = config%test_groups(g)%slow_period
                slow_after = config%test_groups(g)%slow_after
                slow_set = .true.
            end if
        end do
        value = test_retries_override()
        if (value >= 0) retries = value
    end subroutine test_policy

    integer function test_retries_override() result(value)
        !! FO_TEST_RETRIES (`--retries N`), or -1 when unset or invalid.
        character(len=16) :: buf
        integer :: status, ios

        value = cli_retries
        if (value >= 0) return
        call get_environment_variable('FO_TEST_RETRIES', buf, status=status)
        if (status /= 0 .or. len_trim(buf) == 0) return
        read (buf, *, iostat=ios) value
        if (ios /= 0 .or. value < 0) value = -1
    end function test_retries_override

    subroutine test_set_retry_options(retries, strict_flaky)
        !! The command line's `--retries N` (negative: not given) and
        !! `--strict-flaky`; they win over the environment and the manifest.
        integer, intent(in) :: retries
        logical, intent(in) :: strict_flaky

        cli_retries = retries
        cli_strict_flaky = strict_flaky
    end subroutine test_set_retry_options

    logical function test_strict_flaky() result(strict)
        !! FO_TEST_STRICT_FLAKY (`--strict-flaky`): a FLAKY verdict fails the run.
        character(len=8) :: buf
        integer :: status

        strict = cli_strict_flaky
        if (strict) return
        call get_environment_variable('FO_TEST_STRICT_FLAKY', buf, status=status)
        strict = status == 0 .and. len_trim(buf) > 0 .and. buf(1:1) /= '0'
    end function test_strict_flaky

    function timeout_detail(kind, budget, wall_cap, cpu_seconds, wall_seconds) &
            result(text)
        !! One line saying which limit killed a test and what it had used.
        integer, intent(in) :: kind, budget, wall_cap
        real, intent(in) :: cpu_seconds, wall_seconds
        character(len=:), allocatable :: text
        character(len=256) :: buf

        select case (kind)
        case (TIMEOUT_CPU)
            write (buf, '(a,i0,a,f0.1,a,f0.1,a)') 'CPU budget of ', budget, &
                ' s exceeded (', cpu_seconds, ' s CPU in ', wall_seconds, &
                ' s wall)'
        case (TIMEOUT_WALL)
            if (cpu_seconds >= 0.0) then
                write (buf, '(a,i0,a,f0.1,a)') 'wall-clock cap of ', wall_cap, &
                    ' s exceeded; test process itself used only ', cpu_seconds, &
                    ' s CPU (descendant CPU is not included)'
            else
                write (buf, '(a,i0,a)') 'wall-clock cap of ', wall_cap, &
                    ' s exceeded; test-process CPU is unavailable'
            end if
        case (TIMEOUT_UNMEASURED)
            write (buf, '(a,i0,a)') 'budget of ', budget, ' s exceeded in wall '// &
                'time (child CPU time is not measurable on this platform)'
        case default
            write (buf, '(a,i0,a)') 'hard timeout of ', wall_cap, ' s exceeded'
        end select
        text = trim(buf)
    end function timeout_detail

    subroutine environment_seconds(name, seconds)
        !! Replace seconds by a positive integer environment value, if set.
        character(len=*), intent(in) :: name
        integer, intent(inout) :: seconds
        character(len=32) :: buf
        integer :: status, ios, value

        call get_environment_variable(name, buf, status=status)
        if (status /= 0) return
        if (len_trim(buf) == 0) return
        read (buf, *, iostat=ios) value
        if (ios /= 0) return
        if (value >= 1) seconds = value
    end subroutine environment_seconds

end module fo_test_budget
