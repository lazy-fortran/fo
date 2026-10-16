module fo_exec_service
    !! The one asynchronous execution service of fo-owned process trees:
    !! start, poll against an optional wall deadline, and cancel, plus the
    !! coalescing queue of one active run whose newest pending request reruns
    !! after it. Gremlin campaign children, reproductions and MCP asynchronous
    !! requests all execute through it.
    use, intrinsic :: iso_fortran_env, only: int64
    use fo_fs, only: fs_sleep_ms
    use fo_process, only: process_start_argv_logged, process_poll_pid, &
        process_cancel_pid
    implicit none
    private

    public :: exec_job_t, exec_queue_t
    public :: exec_start, exec_poll, exec_cancel, exec_clock_ms
    public :: EXEC_TIMEOUT, EXEC_UNCANCELLED, EXEC_CANCELLED
    public :: RUN_IDLE, RUN_RUNNING, RUN_RERUN_PENDING, RUN_FINISHED

    !! Exit codes of a job the service ended: wall deadline, cancellation that
    !! failed (the tree may still run), and cancellation.
    integer, parameter :: EXEC_TIMEOUT = 124, EXEC_UNCANCELLED = 125
    integer, parameter :: EXEC_CANCELLED = 130
    integer, parameter :: RUN_IDLE = 0, RUN_RUNNING = 1, RUN_RERUN_PENDING = 2
    integer, parameter :: RUN_FINISHED = 3

    type :: exec_job_t
        !! pid > 0 while the owned process tree runs; exitcode once it ended.
        integer :: pid = 0
        integer :: exitcode = 0
        logical :: timed_out = .false.
        integer(int64) :: started_ms = 0_int64
        !! Wall deadline (exec_clock_ms); 0 runs unbounded.
        integer(int64) :: deadline_ms = 0_int64
        character(len=4096) :: log_file = ''
    end type exec_job_t

    type :: exec_queue_t
        !! One active run; requests while it runs coalesce into one pending
        !! rerun that takes the newest root and mode.
        integer :: state = RUN_IDLE
        integer :: last_state = RUN_IDLE
        integer :: started = 0
        integer :: completed = 0
        integer :: last_exitcode = 0
        logical :: rerun_pending = .false.
        character(len=512) :: current_root = '', pending_root = '', last_root = ''
        character(len=32) :: current_mode = '', pending_mode = '', last_mode = ''
    contains
        procedure :: request => queue_request
        procedure :: finish => queue_finish
        procedure :: clear => queue_clear
    end type exec_queue_t

contains

    subroutine exec_start(job, cwd, packed, n_args, log_file, ierr, env, &
            timeout_seconds)
        !! Start argv (argv_push packing) in an owned process session whose
        !! output goes to log_file. ierr is the process setup error.
        type(exec_job_t), intent(out) :: job
        character(len=*), intent(in) :: cwd, packed, log_file
        integer, intent(in) :: n_args
        integer, intent(out) :: ierr
        character(len=*), intent(in), optional :: env
        integer, intent(in), optional :: timeout_seconds

        job%log_file = log_file
        if (present(env)) then
            call process_start_argv_logged(cwd, packed, n_args, log_file, job%pid, &
                ierr, env)
        else
            call process_start_argv_logged(cwd, packed, n_args, log_file, job%pid, ierr)
        end if
        if (ierr /= 0 .or. job%pid <= 0) then
            if (ierr == 0) ierr = 1
            job%pid = 0
            return
        end if
        job%started_ms = exec_clock_ms()
        if (present(timeout_seconds)) then
            if (timeout_seconds > 0) job%deadline_ms = job%started_ms + &
                int(timeout_seconds, int64)*1000_int64
        end if
    end subroutine exec_start

    subroutine exec_poll(job, done, ierr)
        !! done once the tree ended: by itself (its exit code), or at the wall
        !! deadline (EXEC_TIMEOUT). A deadline cancellation that fails sets
        !! ierr, reports EXEC_UNCANCELLED and keeps pid for a later cancel.
        type(exec_job_t), intent(inout) :: job
        logical, intent(out) :: done
        integer, intent(out) :: ierr

        ierr = 0
        done = job%pid <= 0
        if (done) return
        call process_poll_pid(job%pid, done, job%exitcode)
        if (done) then
            job%pid = 0
            return
        end if
        if (job%deadline_ms <= 0_int64 .or. exec_clock_ms() < job%deadline_ms) return
        done = .true.
        job%timed_out = .true.
        call exec_cancel(job, ierr)
        job%exitcode = EXEC_TIMEOUT
        if (ierr /= 0) job%exitcode = EXEC_UNCANCELLED
    end subroutine exec_poll

    subroutine exec_cancel(job, ierr)
        !! Cancel the whole owned tree (bounded retries); a tree that ended
        !! meanwhile counts as cancelled. On success pid is 0.
        type(exec_job_t), intent(inout) :: job
        integer, intent(out) :: ierr
        integer :: attempt, exitcode
        logical :: done

        ierr = 0
        if (job%pid <= 0) return
        do attempt = 1, 3
            call process_cancel_pid(job%pid, ierr)
            if (ierr /= 0) then
                call process_poll_pid(job%pid, done, exitcode)
                if (done) ierr = 0
            end if
            if (ierr == 0) then
                job%pid = 0
                job%exitcode = EXEC_CANCELLED
                return
            end if
            call fs_sleep_ms(50)
        end do
    end subroutine exec_cancel

    integer(int64) function exec_clock_ms() result(milliseconds)
        integer(int64) :: count, rate

        call system_clock(count=count, count_rate=rate)
        milliseconds = 0_int64
        if (rate <= 0_int64) return
        milliseconds = (count/rate)*1000_int64 + modulo(count, rate)*1000_int64/rate
    end function exec_clock_ms

    subroutine queue_request(self, root, mode, ierr)
        class(exec_queue_t), intent(inout) :: self
        character(len=*), intent(in) :: root, mode
        integer, intent(out) :: ierr
        logical :: exists

        ierr = 1
        if (len_trim(root) == 0) return
        inquire (file=trim(root), exist=exists)
        if (.not. exists) return
        ierr = 0
        if (self%state == RUN_IDLE) then
            call queue_start(self, root, mode)
        else
            self%state = RUN_RERUN_PENDING
            self%rerun_pending = .true.
            self%pending_root = root
            self%pending_mode = mode
        end if
    end subroutine queue_request

    subroutine queue_finish(self, exitcode)
        class(exec_queue_t), intent(inout) :: self
        integer, intent(in) :: exitcode
        character(len=512) :: root
        character(len=32) :: mode

        if (self%state /= RUN_RUNNING .and. self%state /= RUN_RERUN_PENDING) return
        self%completed = self%completed + 1
        self%last_exitcode = exitcode
        self%last_state = RUN_FINISHED
        self%last_root = self%current_root
        self%last_mode = self%current_mode
        if (self%rerun_pending) then
            root = self%pending_root
            mode = self%pending_mode
            call queue_start(self, root, mode)
        else
            self%state = RUN_IDLE
            self%current_root = ''
            self%current_mode = ''
        end if
        self%rerun_pending = .false.
        self%pending_root = ''
        self%pending_mode = ''
    end subroutine queue_finish

    subroutine queue_clear(self)
        class(exec_queue_t), intent(inout) :: self

        self%state = RUN_IDLE
        self%rerun_pending = .false.
        self%current_root = ''
        self%current_mode = ''
        self%pending_root = ''
        self%pending_mode = ''
    end subroutine queue_clear

    subroutine queue_start(self, root, mode)
        class(exec_queue_t), intent(inout) :: self
        character(len=*), intent(in) :: root, mode

        self%state = RUN_RUNNING
        self%started = self%started + 1
        self%current_root = root
        self%current_mode = mode
    end subroutine queue_start

end module fo_exec_service
