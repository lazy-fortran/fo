module fo_gremlin_storage
    !! Bounded Gremlin storage. FO_GREMLIN_MAX_SIZE caps the apparent size of
    !! the lane directories and generations below <state base>/fo/gremlin
    !! (default 8G; K/M/G/T suffixes, 0 disables) and FO_GREMLIN_GRACE (seconds,
    !! default 86400) protects everything used that recently. Above the cap the
    !! least recently used dead lanes and stale generations go until 80 % of the
    !! cap remain. Live lanes, leased generations and generations a live or
    !! recently stopped lane names are never candidates; the native scan and
    !! every removal recheck that under the owning lock (fo_gremlin_state.c).
    use, intrinsic :: iso_c_binding, only: c_char, c_int, c_long_long, c_null_char
    use, intrinsic :: iso_fortran_env, only: int64
    use fo_cache, only: cache_env_size
    use fo_gremlin_context, only: common_generation_cas_root
    use fo_gremlin_state, only: gremlin_generation_prune_at
    implicit none
    private

    integer, parameter :: PATH_LEN = 4096, CANDIDATE_LIMIT = 4096
    integer, parameter :: KIND_LANE = 1, KIND_GENERATION = 2, KIND_TOMBSTONE = 3
    integer, parameter :: EBUSY_CODE = 16, ENOENT_CODE = 2
    integer(int64), parameter :: RUN_INTERVAL = 3600_int64

    type, public :: gremlin_storage_report_t
        integer(int64) :: total_bytes = 0_int64
        integer(int64) :: reclaimed_bytes = 0_int64
        integer :: lanes = 0, generations = 0, tombstones = 0, kept = 0
    end type gremlin_storage_report_t

    public :: gremlin_storage_policy, gremlin_storage_root
    public :: gremlin_storage_evict, gremlin_storage_maintain

    interface
        integer(c_int) function c_evict_scan(root, grace, paths, slot_size, limit, &
                kinds, used, bytes, count, total) bind(C, name='fo_gremlin_evict_scan')
            import :: c_char, c_int, c_long_long
            character(kind=c_char), intent(in) :: root(*)
            integer(c_long_long), value :: grace
            character(kind=c_char), intent(inout) :: paths(*)
            integer(c_int), value :: slot_size, limit
            integer(c_int), intent(out) :: kinds(*), count
            integer(c_long_long), intent(out) :: used(*), bytes(*), total
        end function c_evict_scan

        integer(c_int) function c_lane_evict(dir, grace) &
                bind(C, name='fo_gremlin_lane_evict')
            import :: c_char, c_int, c_long_long
            character(kind=c_char), intent(in) :: dir(*)
            integer(c_long_long), value :: grace
        end function c_lane_evict

        integer(c_int) function c_tombstone_remove(path) &
                bind(C, name='fo_gremlin_tombstone_remove')
            import :: c_char, c_int
            character(kind=c_char), intent(in) :: path(*)
        end function c_tombstone_remove

        integer(c_int) function c_unpin_stale(root, grace) &
                bind(C, name='fo_gremlin_generation_unpin_stale')
            import :: c_char, c_int, c_long_long
            character(kind=c_char), intent(in) :: root(*)
            integer(c_long_long), value :: grace
        end function c_unpin_stale

        integer(c_int) function c_evict_due(root, interval) &
                bind(C, name='fx_store_evict_due')
            import :: c_char, c_int, c_long_long
            character(kind=c_char), intent(in) :: root(*)
            integer(c_long_long), value :: interval
        end function c_evict_due
    end interface

contains

    subroutine gremlin_storage_policy(max_bytes, grace)
        integer(int64), intent(out) :: max_bytes, grace
        logical :: ok

        max_bytes = 8_int64 * 1024_int64**3
        grace = 86400_int64
        call cache_env_size('FO_GREMLIN_MAX_SIZE', max_bytes, ok)
        call cache_env_size('FO_GREMLIN_GRACE', grace, ok)
    end subroutine gremlin_storage_policy

    subroutine gremlin_storage_root(root, ierr, message)
        character(len=*), intent(out) :: root
        integer, intent(out) :: ierr
        character(len=*), intent(out) :: message

        call common_generation_cas_root(root, ierr, message)
        if (ierr == 0) root = trim(root)//'/gremlin'
    end subroutine gremlin_storage_root

    subroutine gremlin_storage_maintain(ierr, message)
        !! The periodic run of a lane owner: at most once per RUN_INTERVAL per
        !! Gremlin root (fx's stamp; concurrent runs are safe, only slower).
        integer, intent(out) :: ierr
        character(len=*), intent(out) :: message
        character(len=PATH_LEN) :: root
        integer(int64) :: max_bytes, grace
        type(gremlin_storage_report_t) :: report

        ierr = 0
        message = ''
        call gremlin_storage_policy(max_bytes, grace)
        if (max_bytes <= 0_int64) return
        call gremlin_storage_root(root, ierr, message)
        if (ierr /= 0) return
        if (c_evict_due(trim(root)//c_null_char, int(RUN_INTERVAL, c_long_long)) &
            /= 1_c_int) return
        call gremlin_storage_evict(trim(root), max_bytes, grace, .false., report, &
            ierr, message)
    end subroutine gremlin_storage_maintain

    subroutine gremlin_storage_evict(root, max_bytes, grace, dry_run, report, ierr, &
            message, unit)
        !! One eviction pass over root (a .../fo/gremlin directory). With dry_run
        !! nothing is changed and report/unit show what the pass would reclaim.
        !! Candidates that turn out busy are kept and counted in report%kept.
        character(len=*), intent(in) :: root
        integer(int64), intent(in) :: max_bytes, grace
        logical, intent(in) :: dry_run
        type(gremlin_storage_report_t), intent(out) :: report
        integer, intent(out) :: ierr
        character(len=*), intent(out) :: message
        integer, intent(in), optional :: unit
        character(kind=c_char), allocatable :: paths(:)
        integer(c_int), allocatable :: kinds(:)
        integer(c_long_long), allocatable :: used(:), bytes(:)
        integer(c_long_long) :: total
        integer(c_int) :: count, rc
        integer(int64) :: current, target
        integer :: i, status
        character(len=:), allocatable :: path
        character(len=PATH_LEN) :: detail

        ierr = 0
        message = ''
        if (max_bytes <= 0_int64) return
        allocate (paths(PATH_LEN * CANDIDATE_LIMIT), kinds(CANDIDATE_LIMIT), &
            used(CANDIDATE_LIMIT), bytes(CANDIDATE_LIMIT))
        paths = c_null_char
        rc = c_evict_scan(trim(root)//c_null_char, int(grace, c_long_long), paths, &
            int(PATH_LEN, c_int), int(CANDIDATE_LIMIT, c_int), kinds, used, bytes, &
            count, total)
        if (rc /= 0_c_int) then
            ierr = int(rc)
            write (message, '(a,i0)') 'cannot scan Gremlin storage: error ', ierr
            return
        end if
        report%total_bytes = int(total, int64)
        if (report%total_bytes <= max_bytes) return
        target = max_bytes - max_bytes / 5_int64
        current = report%total_bytes
        do i = 1, int(count)
            if (current <= target) exit
            path = slot_text(paths, i)
            status = 0
            if (.not. dry_run) then
                select case (kinds(i))
                case (KIND_LANE)
                    status = int(c_lane_evict(path//c_null_char, &
                        int(grace, c_long_long)))
                case (KIND_TOMBSTONE)
                    status = int(c_tombstone_remove(path//c_null_char))
                case (KIND_GENERATION)
                    status = int(c_unpin_stale(path//c_null_char, &
                        int(grace, c_long_long)))
                    if (status == 0) call gremlin_generation_prune_at(path, status, &
                        detail)
                end select
            end if
            if (status == EBUSY_CODE .or. status == ENOENT_CODE) then
                report%kept = report%kept + 1
                cycle
            end if
            if (status /= 0) then
                if (ierr == 0) then
                    ierr = status
                    write (message, '(a,i0,a)') 'cannot reclaim Gremlin storage (', &
                        status, ') '//path
                end if
                cycle
            end if
            current = current - int(bytes(i), int64)
            report%reclaimed_bytes = report%reclaimed_bytes + int(bytes(i), int64)
            select case (kinds(i))
            case (KIND_LANE)
                report%lanes = report%lanes + 1
            case (KIND_GENERATION)
                report%generations = report%generations + 1
            case default
                report%tombstones = report%tombstones + 1
            end select
            if (present(unit)) write (unit, '(a,1x,i0,1x,i0,1x,a)') &
                trim(kind_name(int(kinds(i)))), int(bytes(i), int64), &
                int(used(i), int64), path
        end do
    end subroutine gremlin_storage_evict

    function slot_text(buffer, slot) result(text)
        character(kind=c_char), intent(in) :: buffer(:)
        integer, intent(in) :: slot
        character(len=:), allocatable :: text
        integer :: first, n

        first = (slot - 1) * PATH_LEN
        n = 0
        do while (n < PATH_LEN)
            if (buffer(first + n + 1) == c_null_char) exit
            n = n + 1
        end do
        allocate (character(len=n) :: text)
        do n = 1, len(text)
            text(n:n) = buffer(first + n)
        end do
    end function slot_text

    function kind_name(kind) result(name)
        integer, intent(in) :: kind
        character(len=10) :: name

        select case (kind)
        case (KIND_LANE)
            name = 'lane'
        case (KIND_GENERATION)
            name = 'generation'
        case default
            name = 'tombstone'
        end select
    end function kind_name

end module fo_gremlin_storage
