module fo_run_queue
    use fo_json, only: json_bool
    implicit none
    private
    public :: run_queue_t
    public :: RUN_IDLE, RUN_RUNNING, RUN_RERUN_PENDING, RUN_FINISHED
    public :: RUN_ACTIVE

    integer, parameter :: RUN_IDLE = 0
    integer, parameter :: RUN_RUNNING = 1
    integer, parameter :: RUN_RERUN_PENDING = 2
    integer, parameter :: RUN_FINISHED = 3
    integer, parameter :: RUN_ACTIVE = RUN_RUNNING

    type :: run_queue_t
        integer :: state = RUN_IDLE
        integer :: last_state = RUN_IDLE
        integer :: started = 0
        integer :: completed = 0
        integer :: last_exitcode = 0
        logical :: rerun_pending = .false.
        character(len=512) :: current_root = ''
        character(len=32) :: current_mode = ''
        character(len=512) :: pending_root = ''
        character(len=32) :: pending_mode = ''
        character(len=512) :: last_root = ''
        character(len=32) :: last_mode = ''
    contains
        procedure :: request => run_queue_request
        procedure :: finish => run_queue_finish
        procedure :: status_json => run_queue_status_json
    end type run_queue_t

contains

    subroutine run_queue_request(self, root, mode, ierr)
        class(run_queue_t), intent(inout) :: self
        character(len=*), intent(in) :: root, mode
        integer, intent(out) :: ierr

        ierr = 0
        if (.not. valid_root(root)) then
            ierr = 1
            return
        end if

        select case (self%state)
        case (RUN_IDLE)
            call start_run(self, root, mode)
        case (RUN_RUNNING, RUN_RERUN_PENDING)
            self%state = RUN_RERUN_PENDING
            self%rerun_pending = .true.
            self%pending_root = root
            self%pending_mode = mode
        end select
    end subroutine run_queue_request

    subroutine run_queue_finish(self, exitcode)
        class(run_queue_t), intent(inout) :: self
        integer, intent(in) :: exitcode

        if (self%state /= RUN_RUNNING .and. self%state /= RUN_RERUN_PENDING) return

        self%completed = self%completed + 1
        self%last_exitcode = exitcode
        self%last_state = RUN_FINISHED
        self%last_root = self%current_root
        self%last_mode = self%current_mode

        if (self%rerun_pending) then
            call start_run(self, self%pending_root, self%pending_mode)
            self%rerun_pending = .false.
            self%pending_root = ''
            self%pending_mode = ''
        else
            self%state = RUN_IDLE
            self%current_root = ''
            self%current_mode = ''
        end if
    end subroutine run_queue_finish

    subroutine run_queue_status_json(self, line)
        class(run_queue_t), intent(in) :: self
        character(len=*), intent(out) :: line

        character(len=32) :: started, completed, exitcode

        write (started, '(i0)') self%started
        write (completed, '(i0)') self%completed
        write (exitcode, '(i0)') self%last_exitcode

        line = '{'
        select case (self%state)
        case (RUN_RUNNING)
            line = trim(line)//'"state":"running"'
        case (RUN_RERUN_PENDING)
            line = trim(line)//'"state":"rerun-pending"'
        case default
            line = trim(line)//'"state":"idle"'
        end select
        line = trim(line)//',"started":'//trim(started)
        line = trim(line)//',"completed":'//trim(completed)
        line = trim(line)//',"pending":'//trim(json_bool(self%rerun_pending))
        line = trim(line)//',"last_exitcode":'//trim(exitcode)
        line = trim(line)//'}'
    end subroutine run_queue_status_json

    subroutine start_run(self, root, mode)
        class(run_queue_t), intent(inout) :: self
        character(len=*), intent(in) :: root, mode

        self%state = RUN_RUNNING
        self%started = self%started + 1
        self%current_root = root
        self%current_mode = mode
    end subroutine start_run

    logical function valid_root(root)
        character(len=*), intent(in) :: root

        logical :: exists

        valid_root = .false.
        if (len_trim(root) == 0) return
        inquire (file=trim(root), exist=exists)
        valid_root = exists
    end function valid_root

end module fo_run_queue
