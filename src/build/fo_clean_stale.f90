module fo_clean_stale
    !! `fo clean --stale`: delete build artifacts no live target references.
    !!
    !! A content-addressed build never revisits a key once its inputs change, so
    !! ffc's build/ tree had accumulated 224 `objects_*.a` link archives and 9
    !! stale `gfortran_<hash>/` compiler profile dirs beside the ones the
    !! current build uses: 6.5 GB of a 16.8 GB checkout. `fo clean`'s two
    !! existing modes miss it: a plain `clean` drops the whole project tree and
    !! cold-starts everything, and `clean --cache` also purges the shared store
    !! other projects depend on.
    !!
    !! Reference evidence is `build/compile_commands.json`, which names the
    !! include dirs and objects of every target the last real build emitted, plus
    !! the newest `keep` archives, which survive unreferenced because a build
    !! selected by test name links an archive compile_commands does not list.
    use fo_fs, only: fs_collect_files, fs_remove_tree, fs_stat
    use, intrinsic :: iso_fortran_env, only: error_unit
    implicit none
    private

    public :: clean_stale_select, clean_stale_run, clean_stale_plan

    integer, parameter, public :: CLEAN_STALE_DEFAULT_KEEP = 2
    integer, parameter :: MAX_CANDIDATES = 4096

contains

    pure subroutine clean_stale_select(paths, mtimes, sizes, referenced, keep, &
            doomed, n_doomed, freed_bytes)
        !! Pick the paths to delete: unreferenced ones, oldest first, except the
        !! newest `keep` unreferenced ones, which stay so the last build's link
        !! inputs do not have to be rebuilt. Referenced paths never die.
        character(len=512), intent(in) :: paths(:)
        integer(8), intent(in) :: mtimes(:)
        integer(8), intent(in) :: sizes(:)
        logical, intent(in) :: referenced(:)
        integer, intent(in) :: keep
        character(len=512), intent(out) :: doomed(:)
        integer, intent(out) :: n_doomed
        integer(8), intent(out) :: freed_bytes

        integer :: n, i, j, pos
        integer :: order(size(paths)), survivors

        n = size(paths)
        n_doomed = 0
        freed_bytes = 0
        do i = 1, n
            order(i) = i
        end do
        ! insertion sort, newest first: the `keep` survivors are a prefix
        do i = 2, n
            j = order(i)
            pos = i - 1
            do while (pos >= 1)
                if (mtimes(order(pos)) >= mtimes(j)) exit
                order(pos + 1) = order(pos)
                pos = pos - 1
            end do
            order(pos + 1) = j
        end do

        survivors = 0
        do i = 1, n
            j = order(i)
            if (referenced(j)) cycle
            if (survivors < keep) then
                survivors = survivors + 1
                cycle
            end if
            if (n_doomed >= size(doomed)) cycle
            n_doomed = n_doomed + 1
            doomed(n_doomed) = paths(j)
            freed_bytes = freed_bytes + sizes(j)
        end do
    end subroutine clean_stale_select

    subroutine clean_stale_plan(project_dir, keep, paths, sizes, n, freed_bytes)
        !! Enumerate the delete candidates without deleting anything.
        character(len=*), intent(in) :: project_dir
        integer, intent(in) :: keep
        character(len=512), intent(out) :: paths(:)
        integer(8), intent(out) :: sizes(:)
        integer, intent(out) :: n
        integer(8), intent(out) :: freed_bytes

        character(len=512), allocatable :: cand(:), doomed(:)
        integer(8), allocatable :: mtimes(:), csizes(:)
        logical, allocatable :: referenced(:)
        logical :: exists
        character(len=:), allocatable :: text
        integer :: n_cand, i, j, n_doomed

        allocate (cand(MAX_CANDIDATES), doomed(MAX_CANDIDATES))
        allocate (mtimes(MAX_CANDIDATES), csizes(MAX_CANDIDATES))
        allocate (referenced(MAX_CANDIDATES))
        call collect_candidates(project_dir, cand, n_cand)
        call reference_text(project_dir, text)
        ! `referenced` means "named by the last build's compile_commands.json",
        ! not merely "present on disk": fs_stat's ok flag is existence, which
        ! every stale artifact also has.
        referenced = .false.
        do i = 1, n_cand
            call fs_stat(trim(cand(i)), mtimes(i), csizes(i), exists)
            if (.not. exists) csizes(i) = 0
            if (allocated(text)) then
                if (index(text, trim(cand(i))) > 0) referenced(i) = .true.
            end if
        end do
        ! Only the filled prefix is a candidate list: the array is sized for
        ! the worst case, and the empty tail would otherwise be "stale" too.
        call clean_stale_select(cand(1:n_cand), mtimes(1:n_cand), &
            csizes(1:n_cand), referenced(1:n_cand), keep, doomed, n_doomed, &
            freed_bytes)
        n = min(n_doomed, size(paths))
        do i = 1, n
            paths(i) = doomed(i)
            j = index_of(cand, n_cand, doomed(i))
            if (j > 0) sizes(i) = csizes(j)
        end do
    end subroutine clean_stale_plan

    subroutine clean_stale_run(project_dir, keep, n_removed, freed_bytes)
        !! Delete the planned candidates; directories go as trees.
        character(len=*), intent(in) :: project_dir
        integer, intent(in) :: keep
        integer, intent(out) :: n_removed
        integer(8), intent(out) :: freed_bytes

        character(len=512), allocatable :: paths(:)
        integer(8), allocatable :: sizes(:)
        integer :: n, i

        allocate (paths(MAX_CANDIDATES), sizes(MAX_CANDIDATES))
        call clean_stale_plan(project_dir, keep, paths, sizes, n, freed_bytes)
        n_removed = 0
        do i = 1, n
            call fs_remove_tree(trim(paths(i)))
            n_removed = n_removed + 1
        end do
    end subroutine clean_stale_run

    subroutine collect_candidates(project_dir, items, n_items)
        !! Link archives in build/fo/lib and compiler profile dirs in build/.
        character(len=*), intent(in) :: project_dir
        character(len=512), intent(out) :: items(:)
        integer, intent(out) :: n_items

        character(len=512), allocatable :: files(:)
        integer :: n_files, i, ok
        logical :: dummy_ok
        integer(8) :: mtime, size
        character(len=512) :: dir

        n_items = 0
        allocate (files(MAX_CANDIDATES))
        call fs_collect_files(trim(project_dir)//'/build/fo/lib', 'objects_', &
            '.a', '', files, n_files, .false.)
        do i = 1, min(n_files, MAX_CANDIDATES)
            n_items = n_items + 1
            items(n_items) = files(i)
        end do

        ! Compiler profile dirs are build/<compiler>_<hash>/; find them through
        ! a file inside each, then take the directory itself.
        call fs_collect_files(trim(project_dir)//'/build', '', '.mod', &
            '/gfortran_', files, n_files, .true.)
        do i = 1, min(n_files, MAX_CANDIDATES)
            if (n_items >= MAX_CANDIDATES) exit
            call profile_dir(files(i), project_dir, dir, ok)
            if (ok == 0) cycle
            if (index_of(items, n_items, trim(dir)) /= 0) cycle
            call fs_stat(trim(dir), mtime, size, dummy_ok)
            if (.not. dummy_ok) cycle
            n_items = n_items + 1
            items(n_items) = trim(dir)
        end do
    end subroutine collect_candidates

    subroutine profile_dir(path, project_dir, dir, ok)
        !! The `build/<compiler>_<hash>` directory containing `path`.
        character(len=*), intent(in) :: path, project_dir
        character(len=*), intent(out) :: dir
        integer, intent(out) :: ok

        character(len=:), allocatable :: tail
        integer :: cut, prefix

        dir = ''
        ok = 0
        ! '<project>/build/' is the prefix to strip; a path shorter than that
        ! is not in a build tree, and slicing past the end would read memory
        ! that is not there.
        prefix = len_trim(project_dir) + 7
        if (len_trim(path) <= prefix) return
        tail = path(prefix + 1:)
        if (tail(1:9) /= 'gfortran_') return
        cut = index(tail, '/')
        if (cut <= 1) return
        dir = trim(project_dir)//'/build/'//tail(1:cut - 1)
        ok = 1
    end subroutine profile_dir

    pure integer function index_of(items, n, value) result(found)
        character(len=512), intent(in) :: items(:)
        integer, intent(in) :: n
        character(len=*), intent(in) :: value

        integer :: i

        found = 0
        do i = 1, n
            if (trim(items(i)) == trim(value)) then
                found = i
                return
            end if
        end do
    end function index_of

    subroutine reference_text(project_dir, text)
        !! `build/compile_commands.json`, the reference evidence, or unallocated.
        character(len=*), intent(in) :: project_dir
        character(len=:), allocatable :: text

        character(len=4096) :: line
        integer :: unit, ios

        text = ''
        open (newunit=unit, file=trim(project_dir)//'/build/compile_commands.json', &
            status='old', action='read', iostat=ios)
        if (ios /= 0) then
            deallocate (text)
            return
        end if
        do
            read (unit, '(a)', iostat=ios) line
            if (ios /= 0) exit
            text = text//trim(line)//new_line('a')
        end do
        close (unit)
    end subroutine reference_text

end module fo_clean_stale
