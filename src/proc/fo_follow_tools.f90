module fo_follow_tools
    !! Starts fortyard's follow-tools.sh, which keeps fo and fy at main, when it
    !! is installed here and last checked more than five minutes ago. It runs
    !! detached, so this command neither waits for it nor collects it.
    use, intrinsic :: iso_c_binding, only: c_long, c_long_long, c_ptr, c_null_ptr
    use fo_fs, only: fs_stat
    implicit none
    private
    public :: follow_tools_trigger

    integer, parameter :: INTERVAL_S = 300
    interface
        integer(c_long) function c_time(t) bind(C, name='time')
            import :: c_long, c_ptr
            type(c_ptr), value :: t
        end function c_time
    end interface

contains

    subroutine follow_tools_trigger()
        character(len=4096) :: home, value
        character(len=:), allocatable :: state, script
        integer(c_long_long) :: mtime_ns, bytes
        integer :: status
        logical :: found

        call get_environment_variable('FO_NO_FOLLOW', value, status=status)
        if (status == 0 .and. trim(value) == '1') return
        call get_environment_variable('FY_TOOLS_HOME', home, status=status)
        if (status == 0 .and. len_trim(home) > 0) then
            state = trim(home)
        else
            call get_environment_variable('HOME', home, status=status)
            if (status /= 0 .or. len_trim(home) == 0) return
            state = trim(home)//'/.cache/fy-tools'
        end if
        script = state//'/src/fortyard/scripts/follow-tools.sh'
        call fs_stat(script, mtime_ns, bytes, found)
        if (.not. found) return
        call fs_stat(state//'/checked', mtime_ns, bytes, found)
        if (found) then
            if (int(c_time(c_null_ptr), c_long_long) - mtime_ns / 1000000000_c_long_long &
                < INTERVAL_S) return
        end if
        call execute_command_line('{ if command -v setsid >/dev/null 2>&1; then '// &
            'setsid sh "'//script//'"; else sh "'//script//'"; fi; } '// &
            '</dev/null >/dev/null 2>&1 &', wait=.true., exitstat=status)
    end subroutine follow_tools_trigger
end module fo_follow_tools
