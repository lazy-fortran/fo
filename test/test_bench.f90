program test_bench
    use, intrinsic :: iso_fortran_env, only: output_unit, error_unit, real64
    use fo_bench, only: bench_result_t, sort_results_by_mean, discover_bench_targets
    implicit none

    integer :: n_pass, n_fail

    n_pass = 0
    n_fail = 0

    call test_sort_by_mean()
    call test_discover_filters_bench_prefix()
    call report()

contains

    subroutine assert(cond, msg)
        logical, intent(in) :: cond
        character(len=*), intent(in) :: msg

        if (cond) then
            n_pass = n_pass + 1
        else
            n_fail = n_fail + 1
            write (error_unit, '(a)') 'FAIL: '//msg
        end if
    end subroutine assert

    subroutine test_sort_by_mean()
        type(bench_result_t) :: r(3)

        r(1)%target = 'b'; r(1)%mean_ms = 30.0_real64
        r(2)%target = 'a'; r(2)%mean_ms = 10.0_real64
        r(3)%target = 'c'; r(3)%mean_ms = 20.0_real64

        call sort_results_by_mean(r, 3)

        call assert(trim(r(1)%target) == 'a', 'fastest first after sort')
        call assert(trim(r(2)%target) == 'c', 'middle second after sort')
        call assert(trim(r(3)%target) == 'b', 'slowest last after sort')
    end subroutine test_sort_by_mean

    subroutine test_discover_filters_bench_prefix()
        character(len=512) :: dir, targets(8)
        integer :: n_targets, stat

        dir = '/tmp/fo_bench_disc_test'
        call execute_command_line('rm -rf '//trim(dir)//' && mkdir -p '//trim(dir), &
            wait=.true., exitstat=stat)
        call execute_command_line('printf "#!/bin/sh\n" > '//trim(dir)// &
            '/bench_one && chmod +x '//trim(dir)//'/bench_one', &
            wait=.true., exitstat=stat)
        call execute_command_line('printf "#!/bin/sh\n" > '//trim(dir)// &
            '/other && chmod +x '//trim(dir)//'/other', &
            wait=.true., exitstat=stat)

        call discover_bench_targets(trim(dir), targets, n_targets)

        call assert(n_targets == 1, 'discover finds exactly the bench_ target')
        if (n_targets == 1) then
            call assert(trim(targets(1)) == 'bench_one', 'discover returns bench_one')
        end if

        call execute_command_line('rm -rf '//trim(dir), wait=.true., exitstat=stat)
    end subroutine test_discover_filters_bench_prefix

    subroutine report()
        write (output_unit, '(a,i0,a,i0)') 'bench: pass=', n_pass, &
            ' fail=', n_fail
        if (n_fail > 0) stop 1
    end subroutine report

end program test_bench
