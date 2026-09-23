module fo_test_budget
    !! Per-test time limits. A test gets a CPU-time budget, not a wall-clock
    !! one: on a loaded host a test that needs three seconds of CPU can take
    !! thirty of wall time, and whether it passes must not depend on what else
    !! the machine is doing. A much larger wall-clock cap still catches tests
    !! that hang without using CPU (blocked on I/O, sleeping, deadlocked).
    !!
    !! Precedence for each limit: environment variable, then fpm.toml
    !! [extra.fo], then the default.
    use fo_fpm_config, only: fpm_config_t
    use fo_process, only: TIMEOUT_CPU, TIMEOUT_WALL, TIMEOUT_UNMEASURED
    implicit none
    private

    public :: test_timeout_seconds, slow_test_timeout_seconds
    public :: test_budget_seconds, test_wall_cap_seconds
    public :: timeout_detail

    integer, parameter :: DEFAULT_TEST_TIMEOUT = 10
    integer, parameter :: DEFAULT_SLOW_TEST_TIMEOUT = 300
    integer, parameter :: WALL_CAP_FACTOR = 10

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
                    ' s exceeded with only ', cpu_seconds, &
                    ' s CPU used (blocked, sleeping or deadlocked?)'
            else
                write (buf, '(a,i0,a)') 'wall-clock cap of ', wall_cap, &
                    ' s exceeded'
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
