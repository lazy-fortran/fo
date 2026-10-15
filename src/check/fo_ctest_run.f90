module fo_ctest_run
    !! Native execution of CTest cases from the properties `ctest
    !! --show-only=json-v1` reports (#194, #195). A case runs here when every
    !! construct it uses is implemented: argv, WORKING_DIRECTORY, ENVIRONMENT,
    !! TIMEOUT, PASS/FAIL/SKIP regular expressions, WILL_FAIL, SKIP_RETURN_CODE,
    !! REQUIRED_FILES, DISABLED, DEPENDS, FIXTURES_*, RESOURCE_LOCK, RUN_SERIAL
    !! and PROCESSORS. Any other case, and every case related to one by DEPENDS
    !! or a shared fixture, is delegated to ctest (ctest_native_select).
    !!
    !! Verdicts follow cmCTestRunTest: a PASS regex replaces the exit code, a
    !! FAIL regex or a missing PASS match fails, WILL_FAIL inverts that
    !! result, a signal fails even with WILL_FAIL, a skip code or SKIP regex
    !! skips. Results go to the log as the TEST_RESULT lines the fpm runner
    !! writes, with TEST_REASON lines naming why a case failed or did not run.
    !! A fixture setup that did not pass leaves its requirers "Not Run".
    !! Known limit: the process layer reports a signal as 128+signal, so an
    !! exit code in 129..192 is treated as a signal.
    !!
    !! ctest_import_junit turns the JUnit file of a delegated ctest run into
    !! the same lines, so verdicts and names need no console scraping.
    use, intrinsic :: iso_fortran_env, only: int64
    use fx_string, only: string_t
    use fo_util, only: make_tmpfile, read_text_file_alloc
    use fo_fs, only: fs_sleep_ms, fs_find_executable, fs_append_file, fs_remove_file
    use fo_pattern, only: pattern_t, pattern_compile, pattern_matches
    use fo_process, only: argv_push, process_start_argv_logged, &
        process_poll_pid, process_cancel_pid, process_detect_nproc
    use fo_test_budget, only: test_wall_cap_seconds, test_timeout_seconds
    use fo_project_model, only: project_model_t, model_case_t, model_find_case
    implicit none
    private

    public :: ctest_native_select, ctest_native_run, ctest_import_junit
    public :: ctest_log_note

    integer, parameter :: PENDING = 0, RUNNING = 1, FINISHED = 2
    integer, parameter :: MAX_ENV_BYTES = 3000

    type :: run_case_t
        integer :: model_index = 0
        character(len=:), allocatable :: name, cwd, env, exe, reason
        character(len=:), allocatable :: packed
        integer :: n_args = 0
        real :: timeout = 0.0
        logical :: will_fail = .false., disabled = .false., serial = .false.
        logical :: has_skip_code = .false., supported = .false.
        integer :: skip_code = 0, procs = 1
        type(pattern_t), allocatable :: pass_re(:), fail_re(:), skip_re(:)
        type(string_t), allocatable :: required_files(:), locks(:)
        type(string_t), allocatable :: fix_req(:), fix_setup(:), fix_cleanup(:)
        type(string_t), allocatable :: depends(:)
        integer, allocatable :: preds(:), setups(:)
        integer :: state = PENDING
        integer :: pid = 0
        integer(int64) :: t0 = 0
        character(len=4096) :: log = ''
        character(len=10) :: status = ''
        integer :: exit_code = 0
        real :: secs = 0.0
    end type run_case_t

contains

    subroutine ctest_native_select(model, cases, native, reasons)
        !! native(k): cases(k) runs natively. Otherwise reasons(k) says why not.
        type(project_model_t), intent(in) :: model
        integer, intent(in) :: cases(:)
        logical, intent(out) :: native(size(cases))
        type(string_t), intent(out) :: reasons(size(cases))
        type(run_case_t), allocatable :: rcs(:)
        integer, allocatable :: members(:), where(:)
        logical, allocatable :: dirty(:)
        logical :: changed
        integer :: k, i, j, n

        call closure_members(model, cases, members)
        call build_cases(model, members, rcs, where)
        n = size(members)
        allocate (dirty(n))
        dirty = [(.not. rcs(i)%supported, i=1, n)]
        ! A case related to an unsupported one stays with ctest, which owns
        ! the ordering and fixture semantics of the group.
        changed = .true.
        do while (changed)
            changed = .false.
            do i = 1, n
                if (.not. allocated(rcs(i)%preds)) cycle
                do j = 1, size(rcs(i)%preds)
                    k = rcs(i)%preds(j)
                    if (dirty(i) .eqv. dirty(k)) cycle
                    dirty(i) = .true.
                    dirty(k) = .true.
                    changed = .true.
                end do
            end do
        end do
        do k = 1, size(cases)
            i = where(cases(k))
            native(k) = .not. dirty(i)
            reasons(k)%s = ''
            if (native(k)) cycle
            if (allocated(rcs(i)%reason)) then
                reasons(k)%s = rcs(i)%reason
            else
                reasons(k)%s = 'related to a case ctest must run'
            end if
        end do
    end subroutine ctest_native_select

    subroutine ctest_native_run(model, cases, log_file, append, exitcode)
        !! Run cases (all native by ctest_native_select) with their pulled-in
        !! fixture setup and cleanup cases, FO_JOBS wide, and log the results.
        type(project_model_t), intent(in) :: model
        integer, intent(in) :: cases(:)
        character(len=*), intent(in) :: log_file
        logical, intent(in) :: append
        integer, intent(out) :: exitcode
        type(run_case_t), allocatable :: rcs(:)
        integer, allocatable :: members(:), where(:)
        integer :: u, ios

        exitcode = 0
        call closure_members(model, cases, members)
        call build_cases(model, members, rcs, where)
        if (append) then
            open (newunit=u, file=trim(log_file), position='append', &
                status='unknown', action='write', iostat=ios)
        else
            open (newunit=u, file=trim(log_file), status='replace', &
                action='write', iostat=ios)
        end if
        if (ios == 0) then
            write (u, '(a,i0,a)') 'fo: running ', size(rcs), &
                ' CTest case(s) natively (not through ctest)'
            close (u)
        end if
        call schedule(rcs, job_count())
        call report(rcs, log_file, exitcode)
    end subroutine ctest_native_run

    subroutine ctest_log_note(log_file, text)
        character(len=*), intent(in) :: log_file, text
        integer :: u, ios

        open (newunit=u, file=trim(log_file), position='append', &
            status='unknown', action='write', iostat=ios)
        if (ios /= 0) return
        write (u, '(a)') text
        close (u)
    end subroutine ctest_log_note

    integer function job_count() result(jobs)
        character(len=32) :: buf
        integer :: status, ios

        jobs = max(1, process_detect_nproc())
        call get_environment_variable('FO_JOBS', buf, status=status)
        if (status /= 0 .or. len_trim(buf) == 0) return
        read (buf, *, iostat=ios) jobs
        if (ios /= 0 .or. jobs < 1) jobs = max(1, process_detect_nproc())
    end function job_count

    ! ------------------------------------------------------------------
    ! Case construction

    subroutine closure_members(model, cases, members)
        !! The cases plus, as CTest does, the setup and cleanup cases of every
        !! fixture a member requires. Model order.
        type(project_model_t), intent(in) :: model
        integer, intent(in) :: cases(:)
        integer, allocatable, intent(out) :: members(:)
        logical, allocatable :: inc(:)
        type(string_t), allocatable :: req(:)
        logical :: changed
        integer :: i, m, f

        allocate (inc(model%n_cases))
        inc = .false.
        inc(cases) = .true.
        changed = .true.
        do while (changed)
            changed = .false.
            do i = 1, model%n_cases
                if (.not. inc(i)) cycle
                call attr_items(model%cases(i), 'FIXTURES_REQUIRED', req)
                do f = 1, size(req)
                    do m = 1, model%n_cases
                        if (inc(m)) cycle
                        if (.not. (has_item(model%cases(m), 'FIXTURES_SETUP', &
                            req(f)%s) .or. has_item(model%cases(m), &
                            'FIXTURES_CLEANUP', req(f)%s))) cycle
                        inc(m) = .true.
                        changed = .true.
                    end do
                end do
            end do
        end do
        members = pack([(i, i=1, model%n_cases)], inc)
    end subroutine closure_members

    subroutine build_cases(model, members, rcs, where)
        !! Typed cases of the members and their ordering edges. where maps a
        !! model case index to its position in members (0: not a member).
        type(project_model_t), intent(in) :: model
        integer, intent(in) :: members(:)
        type(run_case_t), allocatable, intent(out) :: rcs(:)
        integer, allocatable, intent(out) :: where(:)
        integer :: i, j, k, f, n, count
        integer, allocatable :: buffer(:), setup_buffer(:)

        n = size(members)
        allocate (rcs(n), where(model%n_cases))
        where = 0
        do i = 1, n
            where(members(i)) = i
        end do
        do i = 1, n
            call build_case(model, members(i), rcs(i))
        end do
        do i = 1, n
            allocate (buffer(n), setup_buffer(n))
            count = 0
            k = 0
            do j = 1, size(rcs(i)%depends)
                f = model_find_case(model, rcs(i)%depends(j)%s)
                if (f > 0) then
                    if (where(f) > 0 .and. where(f) /= i) then
                        count = count + 1
                        buffer(count) = where(f)
                    end if
                end if
            end do
            do j = 1, n
                if (j == i) cycle
                if (shares_any(rcs(i)%fix_req, rcs(j)%fix_setup)) then
                    count = count + 1
                    buffer(count) = j
                    k = k + 1
                    setup_buffer(k) = j
                else if (shares_any(rcs(i)%fix_cleanup, rcs(j)%fix_req) .or. &
                    shares_any(rcs(i)%fix_cleanup, rcs(j)%fix_setup)) then
                    count = count + 1
                    buffer(count) = j
                end if
            end do
            rcs(i)%preds = buffer(1:count)
            rcs(i)%setups = setup_buffer(1:k)
            deallocate (buffer, setup_buffer)
        end do
    end subroutine build_cases

    logical function shares_any(a, b) result(shared)
        type(string_t), intent(in) :: a(:), b(:)
        integer :: i, j

        shared = .false.
        do i = 1, size(a)
            do j = 1, size(b)
                if (a(i)%s == b(j)%s) then
                    shared = .true.
                    return
                end if
            end do
        end do
    end function shares_any

    subroutine build_case(model, case_index, rc)
        !! Fill rc from model case index; rc%supported false with rc%reason
        !! when it uses anything not implemented here.
        type(project_model_t), intent(in) :: model
        integer, intent(in) :: case_index
        type(run_case_t), intent(inout) :: rc
        type(string_t), allocatable :: items(:)
        character(len=:), allocatable :: key, exe
        logical :: ok
        integer :: a, k, ios

        associate (c => model%cases(case_index))
            rc%model_index = case_index
            rc%name = c%name
            rc%supported = .false.
            rc%cwd = ''
            rc%env = ''
            allocate (rc%pass_re(0), rc%fail_re(0), rc%skip_re(0))
            allocate (rc%required_files(0), rc%locks(0), rc%fix_req(0), &
                rc%fix_setup(0), rc%fix_cleanup(0), rc%depends(0))
            if (len(c%name) == 0 .or. scan(c%name, ' '//achar(9)) > 0) then
                rc%reason = 'case name is not a single word'
                return
            end if
            if (.not. allocated(c%command)) then
                rc%reason = 'no command argv in the registered case'
                return
            end if
            if (size(c%command) == 0) then
                rc%reason = 'no command argv in the registered case'
                return
            end if
            do k = 1, size(c%command)
                if (len(c%command(k)%s) == 0 .or. &
                    len(c%command(k)%s) /= len_trim(c%command(k)%s)) then
                    rc%reason = 'empty or space-padded command argument'
                    return
                end if
                if (index(c%command(k)%s, '$<') > 0) then
                    rc%reason = 'unexpanded generator expression'
                    return
                end if
            end do
            do a = 1, size(c%attrs)
                key = c%attrs(a)%key
                call attr_items(c, key, items)
                select case (key)
                case ('WORKING_DIRECTORY')
                    if (size(items) /= 1) then
                        rc%reason = 'unsupported WORKING_DIRECTORY'
                        return
                    end if
                    rc%cwd = items(1)%s
                case ('ENVIRONMENT')
                    do k = 1, size(items)
                        if (index(items(k)%s, ';') > 0 .or. &
                            index(items(k)%s, '=') < 2) then
                            rc%reason = 'unsupported ENVIRONMENT entry'
                            return
                        end if
                        if (k > 1) rc%env = rc%env//';'
                        rc%env = rc%env//items(k)%s
                    end do
                    if (len(rc%env) > MAX_ENV_BYTES) then
                        rc%reason = 'ENVIRONMENT too large'
                        return
                    end if
                case ('TIMEOUT')
                    rc%timeout = 0.0
                    if (size(items) == 1) read (items(1)%s, *, iostat=ios) rc%timeout
                    if (size(items) /= 1 .or. ios /= 0 .or. rc%timeout <= 0.0) then
                        rc%reason = 'unsupported TIMEOUT'
                        return
                    end if
                case ('PASS_REGULAR_EXPRESSION')
                    call compile_all(items, rc%pass_re, ok)
                    if (.not. ok) then
                        rc%reason = 'regular expression syntax not supported natively'
                        return
                    end if
                case ('FAIL_REGULAR_EXPRESSION')
                    call compile_all(items, rc%fail_re, ok)
                    if (.not. ok) then
                        rc%reason = 'regular expression syntax not supported natively'
                        return
                    end if
                case ('SKIP_REGULAR_EXPRESSION')
                    call compile_all(items, rc%skip_re, ok)
                    if (.not. ok) then
                        rc%reason = 'regular expression syntax not supported natively'
                        return
                    end if
                case ('WILL_FAIL')
                    rc%will_fail = is_true(items)
                case ('DISABLED')
                    rc%disabled = is_true(items)
                case ('RUN_SERIAL')
                    rc%serial = is_true(items)
                case ('SKIP_RETURN_CODE')
                    if (size(items) == 1) read (items(1)%s, *, iostat=ios) rc%skip_code
                    if (size(items) /= 1 .or. ios /= 0) then
                        rc%reason = 'unsupported SKIP_RETURN_CODE'
                        return
                    end if
                    rc%has_skip_code = .true.
                case ('PROCESSORS')
                    if (size(items) == 1) read (items(1)%s, *, iostat=ios) rc%procs
                    if (size(items) /= 1 .or. ios /= 0 .or. rc%procs < 1) then
                        rc%reason = 'unsupported PROCESSORS'
                        return
                    end if
                case ('REQUIRED_FILES')
                    rc%required_files = items
                case ('RESOURCE_LOCK')
                    rc%locks = items
                case ('FIXTURES_REQUIRED')
                    rc%fix_req = items
                case ('FIXTURES_SETUP')
                    rc%fix_setup = items
                case ('FIXTURES_CLEANUP')
                    rc%fix_cleanup = items
                case ('DEPENDS')
                    rc%depends = items
                case ('COST', '_BACKTRACE_TRIPLES')
                case default
                    rc%reason = 'property '//key//' not implemented natively'
                    return
                end select
            end do
            if (len(rc%cwd) == 0) then
                rc%reason = 'no WORKING_DIRECTORY in the registered case'
                return
            end if
            if (.not. is_directory(rc%cwd)) then
                rc%reason = 'WORKING_DIRECTORY is not a directory'
                return
            end if
            rc%n_args = 0
            rc%packed = ''
            exe = c%command(1)%s
            rc%exe = exe
            do k = 1, size(c%command)
                call argv_push(rc%packed, rc%n_args, c%command(k)%s)
            end do
        end associate
        rc%supported = .true.
        rc%reason = ''
        if (rc%timeout <= 0.0) rc%timeout = real(test_wall_cap_seconds( &
            budget=test_timeout_seconds()))
    end subroutine build_case

    logical function is_directory(path) result(yes)
        character(len=*), intent(in) :: path

        inquire (file=trim(path)//'/.', exist=yes)
    end function is_directory

    logical function is_true(items) result(yes)
        type(string_t), intent(in) :: items(:)
        character(len=:), allocatable :: v
        integer :: k, c

        yes = .false.
        if (size(items) /= 1) return
        v = items(1)%s
        do k = 1, len(v)
            c = iachar(v(k:k))
            if (c >= 65 .and. c <= 90) v(k:k) = achar(c + 32)
        end do
        yes = v == 'true' .or. v == '1' .or. v == 'on' .or. v == 'yes' .or. v == 'y'
    end function is_true

    subroutine compile_all(items, patterns, ok)
        type(string_t), intent(in) :: items(:)
        type(pattern_t), allocatable, intent(out) :: patterns(:)
        logical, intent(out) :: ok
        character(len=:), allocatable :: message
        integer :: k, ierr

        allocate (patterns(size(items)))
        ok = .true.
        do k = 1, size(items)
            call pattern_compile(items(k)%s, patterns(k), ierr, message)
            if (ierr /= 0) ok = .false.
        end do
    end subroutine compile_all

    subroutine attr_items(c, key, items)
        type(model_case_t), intent(in) :: c
        character(len=*), intent(in) :: key
        type(string_t), allocatable, intent(out) :: items(:)
        integer :: a

        do a = 1, size(c%attrs)
            if (c%attrs(a)%key /= key) cycle
            if (allocated(c%attrs(a)%items)) then
                items = c%attrs(a)%items
            else
                allocate (items(1))
                items(1)%s = c%attrs(a)%value
            end if
            return
        end do
        allocate (items(0))
    end subroutine attr_items

    logical function has_item(c, key, value) result(found)
        type(model_case_t), intent(in) :: c
        character(len=*), intent(in) :: key, value
        type(string_t), allocatable :: items(:)
        integer :: k

        found = .false.
        call attr_items(c, key, items)
        do k = 1, size(items)
            if (items(k)%s == value) found = .true.
        end do
    end function has_item

    ! ------------------------------------------------------------------
    ! Scheduling

    subroutine schedule(rcs, jobs)
        type(run_case_t), intent(inout) :: rcs(:)
        integer, intent(in) :: jobs
        integer :: i, n, used, n_running, k
        logical :: progressed, serial_running, started
        character(len=:), allocatable :: blocked

        n = size(rcs)
        used = 0
        n_running = 0
        serial_running = .false.
        do while (any(rcs%state /= FINISHED))
            progressed = .false.
            do i = 1, n
                if (rcs(i)%state /= RUNNING) cycle
                if (poll_case(rcs(i))) then
                    used = used - min(rcs(i)%procs, jobs)
                    n_running = n_running - 1
                    if (rcs(i)%serial) serial_running = .false.
                    progressed = .true.
                end if
            end do
            started = .false.
            do i = 1, n
                if (rcs(i)%state /= PENDING) cycle
                if (.not. all(rcs(rcs(i)%preds)%state == FINISHED)) cycle
                blocked = failed_setups(rcs, i)
                if (len(blocked) > 0) then
                    call finish_untested(rcs(i), 'Failed test dependencies: '//blocked)
                    progressed = .true.
                    cycle
                end if
                if (rcs(i)%disabled) then
                    call finish_instant(rcs(i), 'SKIP', 'CTest test is disabled')
                    progressed = .true.
                    cycle
                end if
                if (.not. resolve_inputs(rcs(i))) then
                    progressed = .true.
                    cycle
                end if
                if (serial_running) cycle
                if (rcs(i)%serial .and. n_running > 0) cycle
                if (n_running > 0 .and. used + min(rcs(i)%procs, jobs) > jobs) cycle
                if (locked(rcs, i)) cycle
                call start_case(rcs(i))
                progressed = .true.
                if (rcs(i)%state == RUNNING) then
                    used = used + min(rcs(i)%procs, jobs)
                    n_running = n_running + 1
                    if (rcs(i)%serial) serial_running = .true.
                    started = .true.
                end if
            end do
            if (progressed .or. started) cycle
            if (n_running > 0) then
                call fs_sleep_ms(5)
            else
                do k = 1, n
                    if (rcs(k)%state == PENDING) call finish_untested(rcs(k), &
                        'dependency cycle between CTest cases')
                end do
            end if
        end do
    end subroutine schedule

    function failed_setups(rcs, i) result(names)
        !! Names of the setup cases of case i that did not pass.
        type(run_case_t), intent(in) :: rcs(:)
        integer, intent(in) :: i
        character(len=:), allocatable :: names
        integer :: k, s

        names = ''
        do k = 1, size(rcs(i)%setups)
            s = rcs(i)%setups(k)
            if (rcs(s)%status == 'PASS' .or. rcs(s)%status == 'SKIP') cycle
            if (len(names) > 0) names = names//', '
            names = names//rcs(s)%name
        end do
    end function failed_setups

    logical function locked(rcs, i) result(busy)
        type(run_case_t), intent(in) :: rcs(:)
        integer, intent(in) :: i
        integer :: j

        busy = .false.
        if (size(rcs(i)%locks) == 0) return
        do j = 1, size(rcs)
            if (j == i .or. rcs(j)%state /= RUNNING) cycle
            if (shares_any(rcs(i)%locks, rcs(j)%locks)) then
                busy = .true.
                return
            end if
        end do
    end function locked

    logical function resolve_inputs(rc) result(ok)
        !! Required files and the executable; finishes rc as Not Run otherwise.
        type(run_case_t), intent(inout) :: rc
        character(len=4096) :: path
        character(len=:), allocatable :: candidate
        logical :: exists
        integer :: k

        ok = .false.
        do k = 1, size(rc%required_files)
            inquire (file=rc%required_files(k)%s, exist=exists)
            if (.not. exists) then
                call finish_untested(rc, 'Unable to find required file: '// &
                    rc%required_files(k)%s)
                return
            end if
        end do
        if (index(rc%exe, '/') > 0) then
            candidate = rc%exe
            if (candidate(1:1) /= '/') candidate = rc%cwd//'/'//candidate
            inquire (file=candidate, exist=exists)
        else
            call fs_find_executable(rc%exe, path, exists)
        end if
        if (.not. exists) then
            call finish_untested(rc, 'Unable to find executable: '//rc%exe)
            return
        end if
        ok = .true.
    end function resolve_inputs

    subroutine start_case(rc)
        type(run_case_t), intent(inout) :: rc
        integer :: ec

        call make_tmpfile('fo-ctest-case', rc%log)
        call process_start_argv_logged(rc%cwd, rc%packed, rc%n_args, trim(rc%log), &
            rc%pid, ec, env_extra=rc%env)
        if (ec /= 0) then
            rc%exit_code = ec
            call finish_instant(rc, 'FAIL', 'cannot start the test process')
            return
        end if
        call system_clock(rc%t0)
        rc%state = RUNNING
    end subroutine start_case

    logical function poll_case(rc) result(done)
        !! Poll a running case; classify and finish it when it ended.
        type(run_case_t), intent(inout) :: rc
        integer :: code, ec
        integer(int64) :: now, rate
        real :: elapsed
        logical :: ended

        call process_poll_pid(rc%pid, ended, code)
        call system_clock(now, rate)
        elapsed = real(now - rc%t0) / real(rate)
        done = .false.
        if (.not. ended) then
            if (elapsed <= rc%timeout) return
            call process_cancel_pid(rc%pid, ec)
            rc%secs = elapsed
            rc%exit_code = 124
            rc%status = 'TIMEOUT'
            rc%reason = 'Timeout'
            rc%state = FINISHED
            done = .true.
            return
        end if
        rc%secs = elapsed
        call classify(rc, code)
        rc%state = FINISHED
        done = .true.
    end function poll_case

    subroutine classify(rc, code)
        !! The CTest verdict of a finished case.
        type(run_case_t), intent(inout) :: rc
        integer, intent(in) :: code
        character(len=:), allocatable :: output
        character(len=16) :: digits
        logical :: force_fail, success, found
        integer :: status

        call read_text_file_alloc(trim(rc%log), output, status)
        if (status /= 0) output = ''
        rc%exit_code = code
        if (code > 128 .and. code <= 192) then
            rc%status = 'FAIL'
            rc%reason = 'Exception: signal'
            return
        end if
        if (rc%has_skip_code .and. code == rc%skip_code) then
            rc%status = 'SKIP'
            rc%reason = 'SKIP_RETURN_CODE='
            return
        end if
        if (any_match(rc%skip_re, output)) then
            rc%status = 'SKIP'
            rc%reason = 'SKIP_REGULAR_EXPRESSION_MATCHED'
            return
        end if
        force_fail = .false.
        rc%reason = ''
        if (size(rc%pass_re) > 0) then
            found = any_match(rc%pass_re, output)
            if (.not. found) then
                force_fail = .true.
                rc%reason = 'Required regular expression not found'
            end if
        end if
        if (size(rc%fail_re) > 0) then
            if (any_match(rc%fail_re, output)) then
                force_fail = .true.
                rc%reason = 'Error regular expression found in output'
            end if
        end if
        success = .not. force_fail .and. (code == 0 .or. size(rc%pass_re) > 0)
        if (rc%will_fail) then
            success = .not. success
            if (.not. success) rc%reason = 'WILL_FAIL case did not fail as required'
        end if
        if (success) then
            rc%status = 'PASS'
            rc%reason = ''
        else
            rc%status = 'FAIL'
            if (len(rc%reason) == 0) then
                write (digits, '(i0)') code
                rc%reason = 'Failed: exit code '//trim(digits)
            end if
        end if
    end subroutine classify

    logical function any_match(patterns, text) result(found)
        type(pattern_t), intent(in) :: patterns(:)
        character(len=*), intent(in) :: text
        integer :: k

        found = .false.
        do k = 1, size(patterns)
            if (pattern_matches(patterns(k), text)) then
                found = .true.
                return
            end if
        end do
    end function any_match

    subroutine finish_instant(rc, status, reason)
        type(run_case_t), intent(inout) :: rc
        character(len=*), intent(in) :: status, reason

        rc%status = status
        rc%reason = reason
        rc%secs = 0.0
        rc%state = FINISHED
    end subroutine finish_instant

    subroutine finish_untested(rc, reason)
        type(run_case_t), intent(inout) :: rc
        character(len=*), intent(in) :: reason

        call finish_instant(rc, 'UNTESTED', reason)
    end subroutine finish_untested

    ! ------------------------------------------------------------------
    ! Reporting

    subroutine report(rcs, log_file, exitcode)
        type(run_case_t), intent(in) :: rcs(:)
        character(len=*), intent(in) :: log_file
        integer, intent(out) :: exitcode
        character(len=16) :: secs_text
        character(len=24) :: exit_text
        integer :: i, u, ios

        exitcode = 0
        do i = 1, size(rcs)
            associate (rc => rcs(i))
                if (len_trim(rc%log) > 0) then
                    if (rc%status == 'FAIL' .or. rc%status == 'TIMEOUT') then
                        call ctest_log_note(log_file, '--- stdout '//rc%name//' ---')
                        call fs_append_file(trim(rc%log), log_file)
                        call ctest_log_note(log_file, '--- end stdout '//rc%name// &
                            ' ---')
                    end if
                    call fs_remove_file(trim(rc%log))
                end if
                if (rc%status == 'FAIL' .or. rc%status == 'TIMEOUT' .or. &
                    rc%status == 'UNTESTED') then
                    exitcode = max(exitcode, 1)
                    if (rc%status == 'TIMEOUT') exitcode = 124
                end if
                exit_text = '-'
                if (rc%status == 'FAIL') write (exit_text, '(i0)') rc%exit_code
                if (rc%status == 'TIMEOUT') exit_text = '124'
                write (secs_text, '(f8.2)') rc%secs
                open (newunit=u, file=trim(log_file), position='append', &
                    status='old', action='write', iostat=ios)
                if (ios /= 0) cycle
                if (len(rc%reason) > 0) write (u, '(a)') 'TEST_REASON '//rc%reason
                write (u, '(a)') 'TEST_RESULT '//rc%name//' '//trim(rc%status)// &
                    ' '//trim(exit_text)//' '//trim(adjustl(secs_text))
                close (u)
            end associate
        end do
    end subroutine report

    ! ------------------------------------------------------------------
    ! JUnit import (delegated ctest runs)

    subroutine ctest_import_junit(junit_file, log_file)
        !! Append the verdicts of a ctest --output-junit file to the log as
        !! TEST_RESULT lines, after a TEST_JUNIT_IMPORT marker that makes the
        !! reader drop the console results of the same run. A missing,
        !! unreadable or unrepresentable file leaves the console to the reader.
        character(len=*), intent(in) :: junit_file, log_file
        character(len=:), allocatable :: text, body, name, status, message, lines, time_text
        integer :: ierr, pos, head_end, close_pos, n
        real :: secs

        call read_text_file_alloc(junit_file, text, ierr)
        if (ierr /= 0) return
        lines = ''
        n = 0
        pos = 1
        do
            head_end = index(text(pos:), '<testcase ')
            if (head_end == 0) exit
            pos = pos + head_end - 1
            head_end = index(text(pos:), '>')
            if (head_end == 0) return
            head_end = pos + head_end - 1
            name = xml_unescape(attribute(text(pos:head_end), 'name'))
            status = attribute(text(pos:head_end), 'status')
            secs = 0.0
            time_text = attribute(text(pos:head_end), 'time')
            read (time_text, *, iostat=ierr) secs
            if (ierr /= 0) secs = 0.0
            if (len(name) == 0 .or. scan(name, ' '//achar(9)//achar(10)) > 0) return
            if (text(head_end - 1:head_end - 1) == '/') then
                body = ''
                close_pos = head_end
            else
                close_pos = index(text(head_end:), '</testcase>')
                if (close_pos == 0) return
                close_pos = head_end + close_pos - 1
                body = text(head_end + 1:close_pos - 1)
            end if
            call junit_verdict(status, body, message)
            lines = lines//junit_lines(name, status, message, body, secs)
            n = n + 1
            pos = close_pos + 1
            if (pos > len(text)) exit
        end do
        if (n == 0) return
        call ctest_log_note(log_file, 'TEST_JUNIT_IMPORT')
        call ctest_log_note(log_file, lines(:len(lines) - 1))
    end subroutine ctest_import_junit

    subroutine junit_verdict(status, body, message)
        !! Map the JUnit status and child message of one case to a verdict.
        character(len=:), allocatable, intent(inout) :: status
        character(len=*), intent(in) :: body
        character(len=:), allocatable, intent(out) :: message

        message = ''
        select case (status)
        case ('run')
            status = 'PASS'
        case ('fail')
            message = attribute(body(max(1, index(body, '<failure')):), 'message')
            status = 'FAIL'
            if (message == 'Timeout') status = 'TIMEOUT'
        case ('disabled')
            status = 'SKIP'
            message = 'CTest test is disabled'
        case default
            message = attribute(body(max(1, index(body, '<skipped')):), 'message')
            status = 'UNTESTED'
            if (index(message, 'SKIP_') == 1) status = 'SKIP'
        end select
    end subroutine junit_verdict

    function junit_lines(name, status, message, body, secs) result(text)
        character(len=*), intent(in) :: name, status, message, body
        real, intent(in) :: secs
        character(len=:), allocatable :: text, reason
        character(len=16) :: secs_text
        character(len=24) :: exit_text
        integer :: first, last

        reason = xml_unescape(message)
        if (status == 'UNTESTED') then
            first = index(body, '<system-out>')
            if (first > 0) then
                first = first + len('<system-out>')
                last = index(body(first:), '</system-out>') + first - 2
                if (last >= first) then
                    reason = xml_unescape(body(first:last))
                    last = index(reason, achar(10))
                    if (last > 0) reason = reason(:last - 1)
                end if
            end if
        end if
        exit_text = '-'
        if (status == 'FAIL') exit_text = '1'
        if (status == 'TIMEOUT') exit_text = '124'
        write (secs_text, '(f8.2)') secs
        text = ''
        if (len_trim(reason) > 0 .and. status /= 'PASS') &
            text = 'TEST_REASON '//trim(reason)//achar(10)
        text = text//'TEST_RESULT '//name//' '//status//' '//trim(exit_text)//' '// &
            trim(adjustl(secs_text))//achar(10)
    end function junit_lines

    function attribute(tag, key) result(value)
        !! Value of key="..." in the first element of tag, '' when absent.
        character(len=*), intent(in) :: tag, key
        character(len=:), allocatable :: value
        integer :: first, last, stop_at

        value = ''
        stop_at = index(tag, '>')
        if (stop_at == 0) stop_at = len(tag)
        first = index(tag(:stop_at), ' '//key//'="')
        if (first == 0) return
        first = first + len(key) + 3
        last = index(tag(first:stop_at), '"')
        if (last == 0) return
        value = tag(first:first + last - 2)
    end function attribute

    function xml_unescape(text) result(plain)
        character(len=*), intent(in) :: text
        character(len=:), allocatable :: plain
        integer :: i, semi, code, ios

        plain = ''
        i = 1
        do while (i <= len(text))
            if (text(i:i) == '&') then
                semi = index(text(i:), ';')
                if (semi > 0) then
                    select case (text(i + 1:i + semi - 2))
                    case ('amp')
                        plain = plain//'&'
                    case ('lt')
                        plain = plain//'<'
                    case ('gt')
                        plain = plain//'>'
                    case ('quot')
                        plain = plain//'"'
                    case ('apos')
                        plain = plain//"'"
                    case default
                        code = -1
                        if (text(i + 1:i + 1) == '#') &
                            read (text(i + 2:i + semi - 2), *, iostat=ios) code
                        if (code >= 0 .and. code < 128) then
                            plain = plain//achar(code)
                        else
                            plain = plain//text(i:i + semi - 1)
                        end if
                    end select
                    i = i + semi
                    cycle
                end if
            end if
            plain = plain//text(i:i)
            i = i + 1
        end do
    end function xml_unescape

end module fo_ctest_run
