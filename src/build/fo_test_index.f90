module fo_test_index
    !! The per-project record of the last test run: for each case its last
    !! status and, for a PASS, the input closure digest the PASS was observed
    !! at (the reuse key of `fo test`, see fo_test_cache). One small text
    !! file in the build tree, replaced atomically; it holds at most one line
    !! per model case (test_index_retain drops removed cases), so it is
    !! bounded by the case count. A missing or damaged file is an empty
    !! index: nothing is reused and no failure is recalled.
    use fx_string, only: string_t
    use fo_project_model, only: project_model_t
    use fo_util, only: read_text_file_alloc, make_sibling_tmpfile, delete_tmpfile
    use fo_fs, only: fs_rename, fs_make_dir
    implicit none
    private

    character(len=*), parameter :: MAGIC = 'fo-test-index-v1'
    integer, parameter :: DIGEST_LEN = 64

    type, public :: test_index_t
        integer :: n = 0
        type(string_t), allocatable :: names(:)
        type(string_t), allocatable :: status(:)
        !! Digest of a PASS; empty for any other status.
        type(string_t), allocatable :: digests(:)
    end type test_index_t

    public :: test_index_dir, test_index_load, test_index_save
    public :: test_index_find, test_index_set, test_index_retain

contains

    function test_index_dir(model, project_dir) result(dir)
        type(project_model_t), intent(in) :: model
        character(len=*), intent(in) :: project_dir
        character(len=:), allocatable :: dir

        dir = trim(project_dir)//'/build'
        if (allocated(model%build_dir)) then
            if (len(model%build_dir) > 0) dir = model%build_dir
        end if
    end function test_index_dir

    subroutine test_index_load(dir, ix)
        character(len=*), intent(in) :: dir
        type(test_index_t), intent(out) :: ix
        character(len=:), allocatable :: text
        integer :: ierr, at, eol, bar1, bar2

        allocate (ix%names(0), ix%status(0), ix%digests(0))
        call read_text_file_alloc(dir//'/fo-test-index', text, ierr)
        if (ierr /= 0) return
        at = index(text, achar(10))
        if (at == 0) return
        if (text(1:at - 1) /= MAGIC) return
        at = at + 1
        do while (at <= len(text))
            eol = index(text(at:), achar(10))
            if (eol == 0) exit
            eol = at + eol - 1
            bar1 = index(text(at:eol - 1), '|')
            bar2 = 0
            if (bar1 > 0) bar2 = index(text(at + bar1:eol - 1), '|')
            if (bar1 > 0 .and. bar2 > 0) then
                bar2 = at + bar1 + bar2 - 1
                bar1 = at + bar1 - 1
                call test_index_set(ix, text(bar2 + 1:eol - 1), &
                    text(at:bar1 - 1), text(bar1 + 1:bar2 - 1))
            end if
            at = eol + 1
        end do
    end subroutine test_index_load

    subroutine test_index_save(dir, ix, ierr)
        !! Atomic replace; ierr /= 0 leaves the previous file in place.
        character(len=*), intent(in) :: dir
        type(test_index_t), intent(in) :: ix
        integer, intent(out) :: ierr
        character(len=4096) :: temporary
        character(len=:), allocatable :: text
        integer :: k, unit, ios

        text = MAGIC//achar(10)
        do k = 1, ix%n
            text = text//ix%status(k)%s//'|'//ix%digests(k)%s//'|'// &
                ix%names(k)%s//achar(10)
        end do
        ierr = 1
        call fs_make_dir(dir)
        call make_sibling_tmpfile(dir//'/fo-test-index', temporary)
        open (newunit=unit, file=trim(temporary), status='new', action='write', &
            access='stream', form='unformatted', iostat=ios)
        if (ios /= 0) return
        write (unit, iostat=ios) text
        close (unit)
        if (ios == 0) ierr = fs_rename(trim(temporary), dir//'/fo-test-index')
        if (ierr /= 0) call delete_tmpfile(trim(temporary))
    end subroutine test_index_save

    integer function test_index_find(ix, name) result(k)
        type(test_index_t), intent(in) :: ix
        character(len=*), intent(in) :: name

        do k = 1, ix%n
            if (ix%names(k)%s == name) return
        end do
        k = 0
    end function test_index_find

    subroutine test_index_set(ix, name, status, digest)
        !! Record status (and the digest of a PASS) of a case. A name with a
        !! line break or a malformed digest is not recorded.
        type(test_index_t), intent(inout) :: ix
        character(len=*), intent(in) :: name, status, digest
        type(string_t), allocatable :: names(:), statuses(:), digests(:)
        integer :: k

        if (len(name) == 0 .or. scan(name, achar(10)//achar(13)) > 0) return
        if (len(digest) /= 0 .and. len(digest) /= DIGEST_LEN) return
        if (len(status) == 0 .or. scan(status, '|'//achar(10)) > 0) return
        k = test_index_find(ix, name)
        if (k == 0) then
            allocate (names(ix%n + 1), statuses(ix%n + 1), digests(ix%n + 1))
            names(1:ix%n) = ix%names(1:ix%n)
            statuses(1:ix%n) = ix%status(1:ix%n)
            digests(1:ix%n) = ix%digests(1:ix%n)
            ix%n = ix%n + 1
            k = ix%n
            call move_alloc(names, ix%names)
            call move_alloc(statuses, ix%status)
            call move_alloc(digests, ix%digests)
            ix%names(k)%s = name
        end if
        ix%status(k)%s = status
        ix%digests(k)%s = digest
    end subroutine test_index_set

    subroutine test_index_retain(ix, model)
        !! Drop the cases the model no longer has.
        use fo_project_model, only: model_find_case
        type(test_index_t), intent(inout) :: ix
        type(project_model_t), intent(in) :: model
        type(test_index_t) :: kept
        integer :: k

        allocate (kept%names(0), kept%status(0), kept%digests(0))
        do k = 1, ix%n
            if (model_find_case(model, ix%names(k)%s) == 0) cycle
            call test_index_set(kept, ix%names(k)%s, ix%status(k)%s, &
                ix%digests(k)%s)
        end do
        ix = kept
    end subroutine test_index_retain

end module fo_test_index
