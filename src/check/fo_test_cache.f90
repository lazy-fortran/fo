module fo_test_cache
    !! The PASS cache of one-shot `fo test` and the MCP test tool, keyed
    !! exactly like Gremlin's case reuse: the input closure digest of the
    !! case (model_case_digests over the project files of the input
    !! inventory) bound to the global inputs (toolchain, driver, every input
    !! outside the project files), computed by fo_gremlin_case_inputs.
    !! A selected case whose digest equals the digest of its recorded PASS
    !! (fo_test_index) is reported `reused` and not run; a build never
    !! stands for a PASS. Only a run that ends in PASS records a digest; a
    !! failing, timed-out or flaky run records its status without one, so it
    !! can never be reused. An unknown or global input (incomplete
    !! inventory, no model digest, no toolchain or driver identity) leaves
    !! the digest empty: the case always runs.
    use fx_hash, only: sha256_file, sha256_string
    use fo_build_backend, only: backend_t
    use fo_project_model, only: project_model_t, model_selection_t, model_find_case
    use fo_project_load, only: project_run_cases
    use fo_gremlin_generation, only: generation_t
    use fo_gremlin_context, only: case_identity_capture
    use fo_gremlin_case_inputs, only: generation_case_digests_ensure
    use fo_test_index, only: test_index_t, test_index_dir, test_index_load, &
        test_index_save, test_index_find, test_index_set, test_index_retain
    use fo_test_results, only: test_result_entry_t, parse_test_results
    use fo_process, only: process_self_executable
    use fo_util, only: make_tmpfile, delete_tmpfile, read_text_file_alloc
    implicit none
    private

    public :: test_cache_run

contains

    subroutine test_cache_run(b, model, cases, flags, no_build, log_file, exitcode, &
            rerun, selection)
        !! project_run_cases for the cases that need to run, plus a REUSED
        !! result line for every case whose recorded PASS is current. rerun
        !! runs every case. log_file receives all results.
        type(backend_t), intent(inout) :: b
        type(project_model_t), intent(in) :: model
        integer, intent(in) :: cases(:)
        character(len=*), intent(in) :: flags, log_file
        logical, intent(in) :: no_build, rerun
        integer, intent(out) :: exitcode
        !! The case filters of executables not listed yet (project_run_cases).
        type(model_selection_t), intent(in), optional :: selection
        character(len=64), allocatable :: digests(:)
        type(test_index_t) :: recorded
        character(len=:), allocatable :: dir, text, reused_lines
        character(len=1024) :: run_log
        integer, allocatable :: to_run(:)
        integer :: k, c, n_run, unit, ios, ierr, clock0, clock1, rate
        logical :: reused

        call system_clock(clock0, rate)
        call model_digests(b, model, flags, digests)
        dir = test_index_dir(model, trim(b%project_dir))
        call test_index_load(dir, recorded)
        allocate (to_run(size(cases)))
        n_run = 0
        reused_lines = ''
        do k = 1, size(cases)
            c = cases(k)
            reused = .false.
            if (.not. rerun .and. len_trim(digests(c)) > 0) then
                ierr = test_index_find(recorded, model%cases(c)%name)
                if (ierr > 0) reused = recorded%status(ierr)%s == 'PASS' .and. &
                    recorded%digests(ierr)%s == trim(digests(c))
            end if
            if (reused) then
                reused_lines = reused_lines//'TEST_RESULT '//model%cases(c)%name// &
                    ' REUSED - 0.00'//achar(10)
            else
                n_run = n_run + 1
                to_run(n_run) = c
            end if
        end do

        exitcode = 0
        if (n_run > 0) then
            call make_tmpfile('fo-test-run', run_log)
            call system_clock(clock0, rate)
            call project_run_cases(b, model, to_run(1:n_run), flags, no_build, &
                trim(run_log), exitcode, selection)
        end if
        open (newunit=unit, file=trim(log_file), position='append', &
            status='unknown', action='write', iostat=ios)
        if (ios == 0) then
            write (unit, '(a)', advance='no') reused_lines
            if (n_run > 0) then
                call system_clock(clock1)
                call read_text_file_alloc(trim(run_log), text, ierr)
                if (ierr == 0) write (unit, '(a)') text
                ! A runner without a run-level clock (CTest) or a record lost
                ! in the log still has the wall time fo measured around it.
                if (.not. has_wall_record(text)) write (unit, '(a,f0.2)') &
                    'TEST_WALL ', real(clock1 - clock0)/real(max(1, rate))
            else
                call system_clock(clock1)
                write (unit, '(a,f0.2)') 'TEST_WALL ', &
                    real(clock1 - clock0)/real(max(1, rate))
            end if
            close (unit)
        end if
        if (n_run == 0) return
        call delete_tmpfile(trim(run_log))
        call record_results(dir, model, digests, log_file)
    end subroutine test_cache_run

    logical function has_wall_record(text)
        !! A `TEST_WALL` record starting a line of the run log.
        character(len=:), allocatable, intent(in) :: text

        has_wall_record = .false.
        if (.not. allocated(text)) return
        has_wall_record = index(text, 'TEST_WALL ') == 1 .or. &
            index(text, achar(10)//'TEST_WALL ') > 0
    end function has_wall_record

    subroutine model_digests(b, model, flags, digests)
        !! Input closure digest of every model case; '' = never reused.
        type(backend_t), intent(inout) :: b
        type(project_model_t), intent(in) :: model
        character(len=*), intent(in) :: flags
        character(len=64), allocatable, intent(out) :: digests(:)
        type(generation_t) :: generation
        character(len=4096) :: executable
        character(len=64) :: driver
        integer :: ierr
        logical :: found, ok

        allocate (digests(model%n_cases))
        digests = ''
        call process_self_executable(executable, found)
        if (.not. found) return
        call sha256_file(trim(executable), driver, ierr)
        if (ierr /= 0) return
        call case_identity_capture(b, driver, flags, generation, ok, configured=.true.)
        if (.not. ok) return
        ! The build profile and --flag text change what the cases run.
        generation%toolchain_key = sha256_string(generation%toolchain_key// &
            '|flags|'//trim(flags))
        allocate (generation%model)
        generation%model = model
        generation%model_root = trim(b%project_dir)
        call generation_case_digests_ensure(generation, trim(b%project_dir))
        if (.not. allocated(generation%case_digests)) return
        digests = generation%case_digests
    end subroutine model_digests

    subroutine record_results(dir, model, digests, log_file)
        !! Record the status of every case of the log: PASS with its digest,
        !! any other verdict without one. Skipped, untested and reused cases
        !! keep their record.
        character(len=*), intent(in) :: dir, log_file
        type(project_model_t), intent(in) :: model
        character(len=64), intent(in) :: digests(:)
        type(test_result_entry_t), allocatable :: entries(:)
        type(test_index_t) :: recorded
        integer :: n_entries, ierr, i, c

        call parse_test_results(log_file, entries, n_entries, ierr)
        if (ierr /= 0) return
        call test_index_load(dir, recorded)
        do i = 1, n_entries
            c = model_find_case(model, trim(entries(i)%name))
            if (c == 0) cycle
            select case (trim(entries(i)%status))
            case ('PASS')
                call test_index_set(recorded, model%cases(c)%name, 'PASS', &
                    trim(digests(c)))
            case ('FAIL', 'TIMEOUT', 'FLAKY')
                call test_index_set(recorded, model%cases(c)%name, &
                    trim(entries(i)%status), '')
            end select
        end do
        call test_index_retain(recorded, model)
        call test_index_save(dir, recorded, ierr)
    end subroutine record_results

end module fo_test_cache
