module fo_bench
    use, intrinsic :: iso_fortran_env, only: output_unit, error_unit, real64
    use fo_build_backend, only: backend_t, backend_build, detect_backend, &
        BACKEND_NONE
    use fo_util, only: make_tmpfile, delete_tmpfile
    use fo_process, only: process_run_logged
    implicit none
    private

    public :: bench_result_t, fo_bench_run
    public :: sort_results_by_mean, discover_bench_targets

    integer, parameter :: MAX_BENCH_TARGETS = 128
    integer, parameter :: MAX_RUNS = 1000
    integer, parameter :: MAX_PATH = 512

    type :: bench_result_t
        character(len=128) :: target = ''
        integer :: n_runs = 0
        real(real64) :: min_ms = 0.0_real64
        real(real64) :: mean_ms = 0.0_real64
        real(real64) :: max_ms = 0.0_real64
    end type bench_result_t

contains

    subroutine fo_bench_run(project_dir, results, n_results, use_json, &
            n_runs, exitcode)
        character(len=*), intent(in) :: project_dir
        type(bench_result_t), intent(out) :: results(:)
        integer, intent(out) :: n_results
        logical, intent(in) :: use_json
        integer, intent(in) :: n_runs
        integer, intent(out) :: exitcode

        type(backend_t) :: b
        character(len=MAX_PATH) :: targets(MAX_BENCH_TARGETS)
        character(len=MAX_PATH) :: bin_dir
        integer :: n_targets, i, build_exit, run_exit
        character(len=512) :: build_log
        logical :: all_ok

        b = detect_backend(project_dir)
        if (b%kind == BACKEND_NONE) then
            write (error_unit, '(a)') 'fo: no fpm.toml or CMakeLists.txt found'
            exitcode = 1
            return
        end if

        call make_tmpfile('fo-bench-build', build_log)
        call backend_build(b, build_exit, log_file=build_log, with_tests=.true.)
        if (build_exit /= 0) then
            write (error_unit, '(a)') 'fo bench: build failed'
            call delete_tmpfile(build_log)
            exitcode = 1
            return
        end if
        call delete_tmpfile(build_log)

        bin_dir = trim(b%project_dir)//'/build/fo/bin'
        call discover_bench_targets(bin_dir, targets, n_targets)

        if (n_targets == 0) then
            n_results = 0
            exitcode = 0
            return
        end if

        n_results = min(n_targets, size(results))
        all_ok = .true.

        do i = 1, n_results
            call run_bench_target(bin_dir, targets(i), n_runs, &
                results(i), run_exit)
            if (run_exit /= 0) then
                all_ok = .false.
            end if
        end do

        call sort_results_by_mean(results, n_results)

        if (use_json) then
            call print_json_results(results, n_results)
        else
            call print_text_results(results, n_results)
        end if

        if (all_ok) then
            exitcode = 0
        else
            exitcode = 1
        end if
    end subroutine fo_bench_run

    subroutine discover_bench_targets(bin_dir, targets, n_targets)
        character(len=*), intent(in) :: bin_dir
        character(len=*), intent(out) :: targets(:)
        integer, intent(out) :: n_targets

        character(len=MAX_PATH) :: entry
        integer :: u, ios, i
        logical :: exists

        n_targets = 0
        inquire (file=trim(bin_dir), exist=exists)
        if (.not. exists) return

        open (newunit=u, file=trim(bin_dir), access='stream', form='unformatted', &
            action='read', iostat=ios)
        if (ios /= 0) then
            call discover_bench_targets_via_ls(bin_dir, targets, n_targets)
            return
        end if
        close (u)

        call discover_bench_targets_via_ls(bin_dir, targets, n_targets)
    end subroutine discover_bench_targets

    subroutine discover_bench_targets_via_ls(bin_dir, targets, n_targets)
        character(len=*), intent(in) :: bin_dir
        character(len=*), intent(out) :: targets(:)
        integer, intent(out) :: n_targets

        character(len=4096) :: list_file
        character(len=512) :: line
        integer :: u, ios

        n_targets = 0
        list_file = trim(bin_dir)//'.bench_list.tmp'

        call execute_command_line( &
            'find '//trim(bin_dir)//&
            ' -maxdepth 1 -type f -name "bench_*" -perm -u+x > '// &
            trim(list_file)//' 2>/dev/null', &
            wait=.true., exitstat=ios)

        open (newunit=u, file=trim(list_file), status='old', &
            action='read', iostat=ios)
        if (ios /= 0) then
            n_targets = 0
            return
        end if

        do while (.true.)
            read (u, '(a)', iostat=ios) line
            if (ios /= 0) exit
            if (len_trim(line) == 0) cycle
            if (n_targets < size(targets)) then
                n_targets = n_targets + 1
                call extract_basename(line, targets(n_targets))
            end if
        end do
        close (u)

        call execute_command_line('rm -f '//trim(list_file), &
            wait=.true., exitstat=ios)
    end subroutine discover_bench_targets_via_ls

    subroutine extract_basename(path, basename)
        character(len=*), intent(in) :: path
        character(len=*), intent(out) :: basename

        integer :: slash

        slash = index(trim(path), '/', back=.true.)
        if (slash > 0) then
            basename = trim(path(slash + 1:))
        else
            basename = trim(path)
        end if
    end subroutine extract_basename

    subroutine run_bench_target(bin_dir, target_name, n_runs, result, exitcode)
        character(len=*), intent(in) :: bin_dir, target_name
        integer, intent(in) :: n_runs
        type(bench_result_t), intent(out) :: result
        integer, intent(out) :: exitcode

        character(len=MAX_PATH) :: bin_path, log_file
        real(real64) :: times(MAX_RUNS)
        integer :: i, run_exit, counts(n_runs), count_rate, count_max
        real(real64) :: elapsed

        bin_path = trim(bin_dir)//'/'//trim(target_name)
        result%target = trim(target_name)
        result%n_runs = n_runs
        result%min_ms = huge(1.0_real64)
        result%max_ms = 0.0_real64
        result%mean_ms = 0.0_real64
        exitcode = 0

        call system_clock(count_rate=count_rate, count_max=count_max)
        if (count_rate <= 0) then
            write (error_unit, '(a)') &
                'fo bench: system_clock not available'
            exitcode = 1
            return
        end if

        do i = 1, n_runs
            call run_bench_once(bin_path, times(i), run_exit)
            if (run_exit /= 0) then
                exitcode = 1
            end if

            if (times(i) < result%min_ms) result%min_ms = times(i)
            if (times(i) > result%max_ms) result%max_ms = times(i)
        end do

        result%mean_ms = sum(times(1:n_runs)) / real(n_runs, real64)
    end subroutine run_bench_target

    subroutine run_bench_once(bin_path, elapsed_ms, exitcode)
        character(len=*), intent(in) :: bin_path
        real(real64), intent(out) :: elapsed_ms
        integer, intent(out) :: exitcode

        integer :: count_start, count_end, count_rate, run_exit
        character(len=512) :: log_file
        real(real64) :: elapsed_sec

        call make_tmpfile('fo-bench-run', log_file)

        call system_clock(count=count_start, count_rate=count_rate)
        call process_run_logged('.', bin_path, log_file, &
            .false., 300, run_exit)
        call system_clock(count=count_end)

        elapsed_sec = real(count_end - count_start, real64) / &
            real(count_rate, real64)
        elapsed_ms = elapsed_sec * 1000.0_real64

        call delete_tmpfile(log_file)

        exitcode = run_exit
    end subroutine run_bench_once

    subroutine sort_results_by_mean(results, n_results)
        type(bench_result_t), intent(inout) :: results(:)
        integer, intent(in) :: n_results

        integer :: i, j
        type(bench_result_t) :: tmp

        do i = 1, n_results - 1
            do j = i + 1, n_results
                if (results(j)%mean_ms < results(i)%mean_ms) then
                    tmp = results(i)
                    results(i) = results(j)
                    results(j) = tmp
                end if
            end do
        end do
    end subroutine sort_results_by_mean

    subroutine print_text_results(results, n_results)
        type(bench_result_t), intent(in) :: results(:)
        integer, intent(in) :: n_results

        integer :: i
        character(len=128) :: fmt

        if (n_results == 0) return

        write (output_unit, '(a)') &
            'target                           runs    min_ms    mean_ms    max_ms'
        write (output_unit, '(a)') &
            '--------------------------------------------------------------------'

        do i = 1, n_results
            write (output_unit, '(a32,i6,3f11.3)') &
                results(i)%target, results(i)%n_runs, &
                results(i)%min_ms, results(i)%mean_ms, results(i)%max_ms
        end do
    end subroutine print_text_results

    subroutine print_json_results(results, n_results)
        type(bench_result_t), intent(in) :: results(:)
        integer, intent(in) :: n_results

        integer :: i

        write (output_unit, '(a)') '['
        do i = 1, n_results
            write (output_unit, '(a)', advance='no') '  {'
            write (output_unit, '(a,a,a)', advance='no') &
                '"target":"', trim(results(i)%target), '"'
            write (output_unit, '(a,i0,a)', advance='no') &
                ',"runs":', results(i)%n_runs, ''
            write (output_unit, '(a,f0.3,a)', advance='no') &
                ',"min_ms":', results(i)%min_ms, ''
            write (output_unit, '(a,f0.3,a)', advance='no') &
                ',"mean_ms":', results(i)%mean_ms, ''
            write (output_unit, '(a,f0.3)', advance='no') &
                ',"max_ms":', results(i)%max_ms
            write (output_unit, '(a)', advance='no') '}'
            if (i < n_results) write (output_unit, '(a)') ','
        end do
        if (n_results > 0) write (output_unit, '(a)') ''
        write (output_unit, '(a)') ']'
    end subroutine print_json_results

end module fo_bench
