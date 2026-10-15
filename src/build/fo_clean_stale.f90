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
    !! Liveness comes from the build's own records, not from path substrings:
    !!  - a link archive or shared library is live while an executable the build
    !!    linked against it still exists (build/fo/link-graph, see fo_link_graph);
    !!  - an object file is live while its source exists, named the way the
    !!    native builder names it (relative path, '/' to '_', plus '.o');
    !!  - a compiler profile dir is live while compile_commands.json names it.
    !! Unreferenced archives and profile dirs keep the newest `keep` entries, so
    !! a build selected by test name does not have to rebuild what the last one
    !! linked. A live project build lock (the build's lease on its outputs)
    !! stops the whole run: nothing is removed under a running build. Deletions
    !! are journalled first, so an interrupted run is finished by the next one.
    use fo_fs, only: fs_collect_files, fs_remove_tree, fs_remove_file, fs_stat, &
        fs_mkdir_excl, fs_pid_alive, fs_write_text
    use fo_link_graph, only: link_graph_load
    use fo_process, only: process_getpid
    use, intrinsic :: iso_fortran_env, only: output_unit
    implicit none
    private

    public :: clean_stale_select, clean_stale_run, clean_stale_plan

    integer, parameter, public :: CLEAN_STALE_DEFAULT_KEEP = 2
    integer, parameter :: LIST_START = 1024
    integer, parameter :: PATH_LEN = 512
    integer, parameter :: CLASS_ARCHIVE = 1, CLASS_PROFILE = 2, CLASS_OBJECT = 3
    character(len=*), parameter :: JOURNAL = '/build/fo/clean-stale.journal'
    character(len=*), parameter :: LOCK_DIR = '/build/fo/.lock'

contains

    pure subroutine clean_stale_select(paths, mtimes, sizes, referenced, keep, &
            doomed, n_doomed, freed_bytes, windowed)
        !! Pick the paths to delete: unreferenced ones, oldest first, except the
        !! newest `keep` unreferenced ones, which stay so the last build's link
        !! inputs do not have to be rebuilt. Referenced paths never die. Entries
        !! with windowed(i) false are not protected by the keep window.
        character(len=PATH_LEN), intent(in) :: paths(:)
        integer(8), intent(in) :: mtimes(:)
        integer(8), intent(in) :: sizes(:)
        logical, intent(in) :: referenced(:)
        integer, intent(in) :: keep
        character(len=PATH_LEN), intent(out) :: doomed(:)
        integer, intent(out) :: n_doomed
        integer(8), intent(out) :: freed_bytes
        logical, intent(in), optional :: windowed(:)

        integer :: n, i, j, pos
        integer :: order(size(paths)), survivors
        logical :: in_window

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
            in_window = .true.
            if (present(windowed)) in_window = windowed(j)
            if (in_window .and. survivors < keep) then
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
        !! Enumerate the delete candidates without deleting anything. `paths`
        !! and `sizes` receive at most their size; `n` is the number stored and
        !! `freed_bytes` counts every candidate.
        character(len=*), intent(in) :: project_dir
        integer, intent(in) :: keep
        character(len=PATH_LEN), intent(out) :: paths(:)
        integer(8), intent(out) :: sizes(:)
        integer, intent(out) :: n
        integer(8), intent(out) :: freed_bytes

        character(len=PATH_LEN), allocatable :: doomed(:)
        integer(8), allocatable :: doomed_sizes(:)
        integer :: n_doomed

        call plan_all(project_dir, keep, doomed, doomed_sizes, n_doomed, &
            freed_bytes)
        n = min(n_doomed, size(paths))
        paths(1:n) = doomed(1:n)
        sizes(1:n) = doomed_sizes(1:n)
    end subroutine clean_stale_plan

    subroutine clean_stale_run(project_dir, keep, n_removed, freed_bytes, &
            verbose, blocked)
        !! Delete the planned candidates; directories go as trees. With
        !! `verbose` every removed path is printed. `blocked` is true when a
        !! live build holds the project lock and nothing was touched.
        character(len=*), intent(in) :: project_dir
        integer, intent(in) :: keep
        integer, intent(out) :: n_removed
        integer(8), intent(out) :: freed_bytes
        logical, intent(in), optional :: verbose
        logical, intent(out), optional :: blocked

        character(len=PATH_LEN), allocatable :: doomed(:)
        integer(8), allocatable :: sizes(:)
        integer :: n, i
        logical :: say, got_lock

        say = .false.
        if (present(verbose)) say = verbose
        n_removed = 0
        freed_bytes = 0
        call acquire_lock(project_dir, got_lock)
        if (present(blocked)) blocked = .not. got_lock
        if (.not. got_lock) return

        call finish_interrupted(project_dir, n_removed, say)
        call plan_all(project_dir, keep, doomed, sizes, n, freed_bytes)
        if (n > 0) call write_journal(project_dir, doomed(1:n))
        do i = 1, n
            call fs_remove_tree(trim(doomed(i)))
            n_removed = n_removed + 1
            if (say) write (output_unit, '(a)') 'removed '//trim(doomed(i))
        end do
        call fs_remove_file(trim(project_dir)//JOURNAL)
        call forget_dead_links(project_dir)
        call release_lock(project_dir)
    end subroutine clean_stale_run

    subroutine plan_all(project_dir, keep, doomed, doomed_sizes, n_doomed, &
            freed_bytes)
        character(len=*), intent(in) :: project_dir
        integer, intent(in) :: keep
        character(len=PATH_LEN), allocatable, intent(out) :: doomed(:)
        integer(8), allocatable, intent(out) :: doomed_sizes(:)
        integer, intent(out) :: n_doomed
        integer(8), intent(out) :: freed_bytes

        character(len=PATH_LEN), allocatable :: cand(:), selected(:)
        integer(8), allocatable :: mtimes(:), csizes(:)
        logical, allocatable :: referenced(:), windowed(:)
        integer, allocatable :: classes(:)
        character(len=:), allocatable :: text
        integer :: n_cand, i, j
        logical :: exists

        call collect_candidates(project_dir, cand, classes, n_cand)
        allocate (mtimes(n_cand), csizes(n_cand), referenced(n_cand), &
            windowed(n_cand), selected(max(n_cand, 1)))
        call reference_text(project_dir, text)
        referenced = .false.
        do i = 1, n_cand
            call fs_stat(trim(cand(i)), mtimes(i), csizes(i), exists)
            if (.not. exists) csizes(i) = 0
        end do
        windowed = classes /= CLASS_OBJECT
        call mark_referenced(project_dir, cand, classes, n_cand, text, referenced)
        call clean_stale_select(cand, mtimes, csizes, referenced, keep, selected, &
            n_doomed, freed_bytes, windowed)
        allocate (doomed(max(n_doomed, 1)), doomed_sizes(max(n_doomed, 1)))
        doomed_sizes = 0
        do i = 1, n_doomed
            doomed(i) = selected(i)
            do j = 1, n_cand
                if (trim(cand(j)) == trim(selected(i))) then
                    doomed_sizes(i) = csizes(j)
                    exit
                end if
            end do
        end do
    end subroutine plan_all

    subroutine mark_referenced(project_dir, cand, classes, n_cand, text, &
            referenced)
        !! Archives: recorded link inputs of an existing executable, or named by
        !! compile_commands.json. Profile dirs: named there. Objects: source exists.
        character(len=*), intent(in) :: project_dir
        character(len=PATH_LEN), intent(in) :: cand(:)
        integer, intent(in) :: classes(:), n_cand
        character(len=:), allocatable, intent(in) :: text
        logical, intent(inout) :: referenced(:)

        character(len=PATH_LEN), allocatable :: outputs(:), inputs(:)
        character(len=PATH_LEN), allocatable :: source_objects(:)
        integer :: n_links, n_sources, i, j
        logical :: output_exists

        call link_graph_load(project_dir, outputs, inputs, n_links)
        call source_object_names(project_dir, source_objects, n_sources)
        do i = 1, n_cand
            ! compile_commands.json records the last build's objects even after
            ! their sources are gone, so it cannot vouch for an object.
            if (allocated(text) .and. classes(i) /= CLASS_OBJECT) then
                if (index(text, trim(cand(i))) > 0) referenced(i) = .true.
            end if
            select case (classes(i))
            case (CLASS_ARCHIVE)
                do j = 1, n_links
                    if (trim(inputs(j)) /= trim(cand(i))) cycle
                    inquire (file=trim(outputs(j)), exist=output_exists)
                    if (output_exists) referenced(i) = .true.
                end do
            case (CLASS_OBJECT)
                ! Without any discovered source the scan itself is suspect.
                if (n_sources == 0) referenced(i) = .true.
                if (is_external_object(cand(i))) referenced(i) = .true.
                do j = 1, n_sources
                    if (trim(source_objects(j)) == trim(base_name(cand(i)))) then
                        referenced(i) = .true.
                        exit
                    end if
                end do
            end select
        end do
    end subroutine mark_referenced

    subroutine collect_candidates(project_dir, items, classes, n_items)
        !! Link archives and orphan-checked objects under build/fo, and
        !! compiler profile dirs in build/.
        character(len=*), intent(in) :: project_dir
        character(len=PATH_LEN), allocatable, intent(out) :: items(:)
        integer, allocatable, intent(out) :: classes(:)
        integer, intent(out) :: n_items

        character(len=PATH_LEN), allocatable :: files(:)
        integer :: n_files, i, ok
        logical :: dummy_ok
        integer(8) :: mtime, size
        character(len=PATH_LEN) :: dir

        n_items = 0
        allocate (items(LIST_START), classes(LIST_START))
        call collect(trim(project_dir)//'/build/fo/lib', 'objects_', '.a', '', &
            .false., files, n_files)
        do i = 1, n_files
            call push(items, classes, n_items, files(i), CLASS_ARCHIVE)
        end do
        call collect(trim(project_dir)//'/build/fo/obj', '', '.o', '', &
            .false., files, n_files)
        do i = 1, n_files
            call push(items, classes, n_items, files(i), CLASS_OBJECT)
        end do

        ! Compiler profile dirs are build/<compiler>_<hash>/; find them through
        ! a file inside each, then take the directory itself.
        call collect(trim(project_dir)//'/build', '', '.mod', '/gfortran_', &
            .true., files, n_files)
        do i = 1, n_files
            call profile_dir(files(i), project_dir, dir, ok)
            if (ok == 0) cycle
            if (index_of(items, classes, n_items, trim(dir), CLASS_PROFILE) /= 0) &
                cycle
            call fs_stat(trim(dir), mtime, size, dummy_ok)
            if (.not. dummy_ok) cycle
            call push(items, classes, n_items, trim(dir), CLASS_PROFILE)
        end do
        items = items(1:n_items)
        classes = classes(1:n_items)
    end subroutine collect_candidates

    subroutine source_object_names(project_dir, names, n_names)
        !! Object file names every source under the project would compile to.
        character(len=*), intent(in) :: project_dir
        character(len=PATH_LEN), allocatable, intent(out) :: names(:)
        integer, intent(out) :: n_names

        character(len=8), parameter :: SUFFIXES(*) = [character(len=8) :: &
            '.f90', '.F90', '.f', '.F', '.for', '.f03', '.F03', '.f08', '.F08', &
            '.c', '.cc', '.cpp', '.cxx', '.C']
        character(len=PATH_LEN), allocatable :: files(:)
        integer :: n_files, i, k, prefix
        character(len=PATH_LEN) :: rel

        n_names = 0
        allocate (names(LIST_START))
        prefix = len_trim(project_dir) + 2
        do k = 1, size(SUFFIXES)
            call collect(trim(project_dir), '', trim(SUFFIXES(k)), '', .true., &
                files, n_files)
            do i = 1, n_files
                if (len_trim(files(i)) < prefix) cycle
                rel = files(i)(prefix:)
                call flatten(rel)
                if (n_names >= size(names)) call grow(names)
                n_names = n_names + 1
                names(n_names) = trim(rel)//'.o'
            end do
        end do
    end subroutine source_object_names

    subroutine finish_interrupted(project_dir, n_removed, say)
        !! A journal left behind by a killed run lists paths that run chose to
        !! delete. Re-check each against the current graph (keep window off) and
        !! finish only the ones still unreferenced.
        character(len=*), intent(in) :: project_dir
        integer, intent(inout) :: n_removed
        logical, intent(in) :: say

        character(len=PATH_LEN), allocatable :: listed(:), doomed(:)
        integer(8), allocatable :: sizes(:)
        integer :: n_listed, n_doomed, i, j, unit, ios
        integer(8) :: freed
        character(len=PATH_LEN) :: line

        n_listed = 0
        allocate (listed(LIST_START))
        open (newunit=unit, file=trim(project_dir)//JOURNAL, status='old', &
            action='read', iostat=ios)
        if (ios /= 0) return
        do
            read (unit, '(a)', iostat=ios) line
            if (ios /= 0) exit
            if (len_trim(line) == 0) cycle
            if (n_listed >= size(listed)) call grow(listed)
            n_listed = n_listed + 1
            listed(n_listed) = line
        end do
        close (unit)
        call plan_all(project_dir, 0, doomed, sizes, n_doomed, freed)
        do i = 1, n_listed
            do j = 1, n_doomed
                if (trim(doomed(j)) /= trim(listed(i))) cycle
                call fs_remove_tree(trim(listed(i)))
                n_removed = n_removed + 1
                if (say) write (output_unit, '(a)') &
                    'removed (interrupted run) '//trim(listed(i))
                exit
            end do
        end do
        call fs_remove_file(trim(project_dir)//JOURNAL)
    end subroutine finish_interrupted

    subroutine write_journal(project_dir, doomed)
        character(len=*), intent(in) :: project_dir
        character(len=PATH_LEN), intent(in) :: doomed(:)

        integer :: unit, ios, i

        open (newunit=unit, file=trim(project_dir)//JOURNAL, status='replace', &
            action='write', iostat=ios)
        if (ios /= 0) return
        do i = 1, size(doomed)
            write (unit, '(a)') trim(doomed(i))
        end do
        close (unit)
    end subroutine write_journal

    subroutine forget_dead_links(project_dir)
        !! Drop recorded links whose executable is gone.
        character(len=*), intent(in) :: project_dir

        character(len=PATH_LEN), allocatable :: outputs(:), inputs(:)
        integer :: n, i
        logical :: exists

        call link_graph_load(project_dir, outputs, inputs, n)
        do i = 1, n
            inquire (file=trim(outputs(i)), exist=exists)
            if (.not. exists) then
                call rewrite_links(project_dir, outputs, inputs, n)
                return
            end if
        end do
    end subroutine forget_dead_links

    subroutine rewrite_links(project_dir, outputs, inputs, n)
        character(len=*), intent(in) :: project_dir
        character(len=PATH_LEN), intent(in) :: outputs(:), inputs(:)
        integer, intent(in) :: n

        character(len=:), allocatable :: text
        integer :: i
        logical :: exists

        text = ''
        do i = 1, n
            inquire (file=trim(outputs(i)), exist=exists)
            if (exists) text = text//trim(outputs(i))//achar(9)// &
                trim(inputs(i))//achar(10)
        end do
        call fs_write_text(trim(project_dir)//'/build/fo/link-graph', text)
    end subroutine rewrite_links

    subroutine acquire_lock(project_dir, got)
        !! The build lock is the lease on the project's build outputs: refuse
        !! while its owner lives, take over a dead owner's lock.
        character(len=*), intent(in) :: project_dir
        logical, intent(out) :: got

        character(len=PATH_LEN) :: dir
        integer :: state, unit, ios, owner, attempt

        got = .false.
        dir = trim(project_dir)//LOCK_DIR
        do attempt = 1, 3
            state = fs_mkdir_excl(trim(dir))
            if (state == 0) exit
            if (state < 0) return
            owner = 0
            open (newunit=unit, file=trim(dir)//'/pid', status='old', &
                action='read', iostat=ios)
            if (ios == 0) then
                read (unit, *, iostat=ios) owner
                close (unit)
            end if
            if (owner <= 0 .or. fs_pid_alive(owner)) return
            call fs_remove_tree(trim(dir))
            if (attempt == 3) return
        end do
        if (state /= 0) return
        open (newunit=unit, file=trim(dir)//'/pid', status='replace', &
            action='write', iostat=ios)
        if (ios == 0) then
            write (unit, '(i0)') process_getpid()
            close (unit)
        end if
        got = .true.
    end subroutine acquire_lock

    subroutine release_lock(project_dir)
        character(len=*), intent(in) :: project_dir

        call fs_remove_tree(trim(project_dir)//LOCK_DIR)
    end subroutine release_lock

    subroutine collect(dir, infix, suffix, needle, recursive, files, n_files)
        !! fs_collect_files without a fixed cap: retry with a larger list.
        character(len=*), intent(in) :: dir, infix, suffix, needle
        logical, intent(in) :: recursive
        character(len=PATH_LEN), allocatable, intent(out) :: files(:)
        integer, intent(out) :: n_files

        integer :: ierr, capacity

        capacity = LIST_START
        do
            allocate (files(capacity))
            call fs_collect_files(dir, infix, suffix, needle, files, n_files, &
                recursive, ierr)
            if (ierr == 0) exit
            deallocate (files)
            if (capacity > 1000000000) then
                allocate (files(0))
                n_files = 0
                return
            end if
            capacity = capacity * 4
        end do
    end subroutine collect

    subroutine push(items, classes, n, value, class)
        character(len=PATH_LEN), allocatable, intent(inout) :: items(:)
        integer, allocatable, intent(inout) :: classes(:)
        integer, intent(inout) :: n
        character(len=*), intent(in) :: value
        integer, intent(in) :: class

        integer, allocatable :: grown(:)

        if (n >= size(items)) then
            call grow(items)
            allocate (grown(2 * size(classes)))
            grown(1:n) = classes(1:n)
            call move_alloc(grown, classes)
        end if
        n = n + 1
        items(n) = value
        classes(n) = class
    end subroutine push

    subroutine grow(items)
        character(len=PATH_LEN), allocatable, intent(inout) :: items(:)

        character(len=PATH_LEN), allocatable :: bigger(:)

        allocate (bigger(2 * size(items)))
        bigger(1:size(items)) = items
        call move_alloc(bigger, items)
    end subroutine grow

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
        tail = trim(path(prefix + 1:))
        if (len(tail) < 9) return
        if (tail(1:9) /= 'gfortran_') return
        cut = index(tail, '/')
        if (cut <= 1) return
        dir = trim(project_dir)//'/build/'//tail(1:cut - 1)
        ok = 1
    end subroutine profile_dir

    pure integer function index_of(items, classes, n, value, class) result(found)
        character(len=PATH_LEN), intent(in) :: items(:)
        integer, intent(in) :: classes(:), n, class
        character(len=*), intent(in) :: value

        integer :: i

        found = 0
        do i = 1, n
            if (classes(i) == class .and. trim(items(i)) == trim(value)) then
                found = i
                return
            end if
        end do
    end function index_of

    pure function base_name(path) result(name)
        character(len=*), intent(in) :: path
        character(len=len(path)) :: name
        integer :: slash

        slash = index(trim(path), '/', back=.true.)
        name = path(slash + 1:)
    end function base_name

    pure logical function is_external_object(path) result(yes)
        character(len=*), intent(in) :: path
        character(len=len(path)) :: name

        name = base_name(path)
        yes = name(1:min(9, len(name))) == 'external_'
    end function is_external_object

    pure subroutine flatten(rel)
        character(len=*), intent(inout) :: rel
        integer :: i

        do i = 1, len_trim(rel)
            if (rel(i:i) == '/') rel(i:i) = '_'
        end do
    end subroutine flatten

    subroutine reference_text(project_dir, text)
        !! `build/compile_commands.json`, one piece of reference evidence, or
        !! unallocated.
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
