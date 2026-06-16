module fo_progress
    !! Compact, throttled build/test progress on stderr.
    !!
    !! Designed to add no measurable cost to the parallel compile/test loops:
    !!   - the per-item hot path is a single `!$omp atomic` increment plus one
    !!     system_clock read and a compare; it takes no lock and never blocks;
    !!   - an actual redraw happens at most once per ~0.1 s (1 s when stderr is
    !!     not a terminal), inside a short critical section a thread reaches only
    !!     when that interval has elapsed, so the workers are never serialized;
    !!   - output is a raw write(2) to fd 2 (process_write_stderr), never a fork,
    !!     because forking from a multithreaded region corrupts libgomp.
    !!
    !! On a TTY the line is an in-place animated bar; otherwise it is a plain
    !! line every interval (parseable: "fo <label> [..] done/total pct%").
    !! Set FO_NO_PROGRESS to disable entirely.
    use, intrinsic :: iso_fortran_env, only: int64
    use fo_process, only: process_stderr_is_tty, process_write_stderr
    implicit none
    private
    public :: progress_begin, progress_step, progress_end

    integer :: g_total = 0
    integer :: g_done = 0
    integer(int64) :: g_last = 0
    integer(int64) :: g_rate = 0
    real :: g_interval = 0.1
    integer :: g_shown = -1
    logical :: g_active = .false.
    logical :: g_tty = .false.
    character(len=16) :: g_label = ''

contains

    subroutine progress_begin(label, total)
        !! Start a progress phase of `total` items. No-op when total <= 0 or
        !! FO_NO_PROGRESS is set.
        character(len=*), intent(in) :: label
        integer, intent(in) :: total
        character(len=8) :: off
        integer :: ln, st

        g_active = .false.
        if (total <= 0) return
        call get_environment_variable('FO_NO_PROGRESS', off, ln, st)
        if (st == 0 .and. ln > 0) return

        g_tty = process_stderr_is_tty()
        g_total = total
        g_done = 0
        g_label = label
        g_interval = 0.1
        if (.not. g_tty) g_interval = 1.0
        g_shown = -1
        call system_clock(g_last, g_rate)
        g_active = .true.
        call render(0)
    end subroutine progress_begin

    subroutine progress_step()
        !! Mark one item complete. Lock-free except for the rare throttled redraw.
        integer(int64) :: now
        real :: dt
        integer :: snap

        if (.not. g_active) return
        !$omp atomic
        g_done = g_done + 1
        if (g_rate <= 0) return
        call system_clock(now)
        dt = real(now - g_last) / real(g_rate)
        if (dt < g_interval) return
        !$omp critical (fo_progress)
        call system_clock(now)
        dt = real(now - g_last) / real(g_rate)
        if (dt >= g_interval) then
            g_last = now
            snap = g_done
            call render(snap)
        end if
        !$omp end critical (fo_progress)
    end subroutine progress_step

    subroutine progress_end()
        !! Finish the phase: draw the full bar and close the line.
        if (.not. g_active) return
        call render(g_total)
        if (g_tty) call process_write_stderr(new_line('a'))
        g_active = .false.
    end subroutine progress_end

    subroutine render(done)
        integer, intent(in) :: done
        integer, parameter :: BARW = 20
        character(len=BARW) :: bar
        character(len=160) :: line
        integer :: pct, filled, i, d

        d = done
        if (d > g_total) d = g_total
        if (d < 0) d = 0
        if (d == g_shown) return  ! skip a redundant redraw (e.g. final 100%)
        g_shown = d
        pct = 0
        if (g_total > 0) pct = int(100.0 * real(d) / real(g_total))
        filled = (pct * BARW) / 100
        do i = 1, BARW
            if (i <= filled) then
                bar(i:i) = '#'
            else
                bar(i:i) = '-'
            end if
        end do
        write (line, '(a,1x,a,1x,a,1x,i0,a,i0,1x,i0,a)') &
            'fo', trim(g_label), '['//bar//']', d, '/', g_total, pct, '%'

        if (g_tty) then
            ! carriage return, the line, then clear to end of line (ESC[K)
            call process_write_stderr(achar(13)//trim(line)//achar(27)//'[K')
        else
            call process_write_stderr(trim(line)//new_line('a'))
        end if
    end subroutine render

end module fo_progress
