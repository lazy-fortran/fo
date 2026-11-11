module fo_compile_inputs
    !! Files a gfortran compile of an fpm source reads that the source-tree
    !! digest of fx does not cover: includes as the compiler itself resolves
    !! them, and module files lying next to the source.
    use fo_cache, only: HASH_LEN, cache_file_digest
    use fo_fs, only: fs_collect_files
    use fo_util, only: read_text_file_alloc
    use fx_hash, only: sha256_string
    implicit none
    private

    integer, parameter :: MAX_INCLUDE_DEPTH = 16
    integer, parameter :: INCLUDE_NONE = 0, INCLUDE_FORTRAN = 1, &
        INCLUDE_QUOTE = 2, INCLUDE_ANGLE = 3
    character(len=1), parameter :: nl = achar(10)

    type, public :: include_closure_t
        !! Name and content of every file the include lines resolve to, blank
        !! without includes.
        character(len=HASH_LEN) :: digest = ''
        !! The compiler reads another file than the source-tree digest holds.
        logical :: differs = .false.
        !! An include was not found without the compiler's own search list.
        logical :: unresolved = .false.
        !! __DATE__, __TIME__ or __TIMESTAMP__ occurs: no two compiles agree.
        logical :: time_macros = .false.
    end type include_closure_t

    public :: include_closure, source_dir_modules_digest, project_relative

contains

    subroutine include_closure(source, root, dirs, chain, chain_known, closure)
        !! The includes of source as gfortran resolves them. A Fortran include
        !! is searched in the directory of source and then in dirs, at any
        !! depth; `#include "x"` next to the including file, then in dirs and
        !! chain; `#include <x>` in dirs and chain. dirs are the include
        !! directories of the command in their order, chain the compiler's
        !! complete search list (relative entries below root), valid when
        !! chain_known.
        character(len=*), intent(in) :: source, root
        character(len=*), intent(in) :: dirs(:), chain(:)
        logical, intent(in) :: chain_known
        type(include_closure_t), intent(out) :: closure
        character(len=:), allocatable :: record

        record = ''
        call walk(source, directory_of(source), root, dirs, chain, chain_known, 0, &
            record, closure)
        if (len(record) > 0) closure%digest = sha256_string(record)
    end subroutine include_closure

    recursive subroutine walk(file, main_dir, root, dirs, chain, chain_known, depth, &
            record, closure)
        character(len=*), intent(in) :: file, main_dir, root
        character(len=*), intent(in) :: dirs(:), chain(:)
        logical, intent(in) :: chain_known
        integer, intent(in) :: depth
        character(len=:), allocatable, intent(inout) :: record
        type(include_closure_t), intent(inout) :: closure
        character(len=:), allocatable :: text, name, found, expected
        character(len=HASH_LEN) :: content
        integer :: ierr, pos, line_end, kind

        call read_text_file_alloc(file, text, ierr)
        if (ierr /= 0) return
        if (index(text, '__DATE__') > 0 .or. index(text, '__TIME__') > 0 .or. &
            index(text, '__TIMESTAMP__') > 0) closure%time_macros = .true.
        if (depth >= MAX_INCLUDE_DEPTH) return
        pos = 1
        do while (pos <= len(text))
            line_end = index(text(pos:), nl)
            if (line_end == 0) then
                line_end = len(text)
            else
                line_end = pos + line_end - 2
            end if
            call parse_include(text(pos:line_end), name, kind)
            pos = line_end + 2
            if (kind == INCLUDE_NONE) cycle
            call resolve(file, main_dir, root, dirs, chain, chain_known, name, kind, &
                found, expected, closure%unresolved)
            if (found /= expected) closure%differs = .true.
            if (len(found) == 0) then
                record = record//'missing '//name//nl
                cycle
            end if
            call cache_file_digest(found, content)
            record = record//project_relative(found, root)//' '//content//nl
            call walk(found, main_dir, root, dirs, chain, chain_known, depth + 1, &
                record, closure)
        end do
    end subroutine walk

    subroutine resolve(file, main_dir, root, dirs, chain, chain_known, name, kind, &
            found, expected, unresolved)
        !! found: the file the compiler reads, expected: the one the
        !! source-tree digest of fx holds; blank when there is none.
        character(len=*), intent(in) :: file, main_dir, root, name
        character(len=*), intent(in) :: dirs(:), chain(:)
        logical, intent(in) :: chain_known
        integer, intent(in) :: kind
        character(len=:), allocatable, intent(out) :: found, expected
        logical, intent(inout) :: unresolved
        character(len=:), allocatable :: in_dirs, candidate
        integer :: i

        found = ''
        expected = ''
        if (name(1:1) == '/') then
            if (exists(name)) found = name
            expected = found
            return
        end if
        in_dirs = ''
        do i = 1, size(dirs)
            candidate = trim(dirs(i))//'/'//name
            if (.not. exists(candidate)) cycle
            in_dirs = candidate
            exit
        end do
        expected = in_dirs
        if (kind /= INCLUDE_ANGLE) then
            candidate = directory_of(file)//name
            if (exists(candidate)) expected = candidate
        end if
        found = in_dirs
        select case (kind)
        case (INCLUDE_FORTRAN)
            candidate = main_dir//name
            if (exists(candidate)) found = candidate
        case (INCLUDE_QUOTE)
            found = expected
        end select
        if (len(found) > 0) return
        if (.not. chain_known) unresolved = .true.
        do i = 1, size(chain)
            candidate = trim(chain(i))//'/'//name
            if (candidate(1:1) /= '/') candidate = trim(root)//'/'//candidate
            if (.not. exists(candidate)) cycle
            found = candidate
            return
        end do
    end subroutine resolve

    subroutine parse_include(line, name, kind)
        !! A Fortran `include 'x'` or a preprocessor `#include "x"`/`<x>`.
        character(len=*), intent(in) :: line
        character(len=:), allocatable, intent(out) :: name
        integer, intent(out) :: kind
        character(len=1) :: closing
        character(len=7) :: word
        integer :: i, k
        logical :: directive

        name = ''
        kind = INCLUDE_NONE
        i = verify(line, ' '//achar(9))
        if (i == 0) return
        if (scan(line(i:i), '#iI') == 0) return
        directive = line(i:i) == '#'
        if (directive) then
            k = verify(line(i + 1:), ' '//achar(9))
            if (k == 0) return
            i = i + k
        end if
        if (len(line) < i + 7) return
        word = line(i:i + 6)
        do k = 1, len(word)
            if (word(k:k) >= 'A' .and. word(k:k) <= 'Z') &
                word(k:k) = achar(iachar(word(k:k)) + 32)
        end do
        if (word /= 'include') return
        i = i + 7
        if (scan(line(i:i), ' '//achar(9)//'''"<') == 0) return
        k = verify(line(i:), ' '//achar(9))
        if (k == 0) return
        i = i + k - 1
        select case (line(i:i))
        case ('''', '"')
            closing = line(i:i)
        case ('<')
            if (.not. directive) return
            closing = '>'
        case default
            return
        end select
        k = index(line(i + 1:), closing)
        if (k < 2) return
        name = line(i + 1:i + k - 1)
        kind = INCLUDE_FORTRAN
        if (directive) kind = merge(INCLUDE_ANGLE, INCLUDE_QUOTE, closing == '>')
    end subroutine parse_include

    function source_dir_modules_digest(dir) result(digest)
        !! Name and content of the module files in dir, blank without any.
        !! gfortran reads a module from the directory of the source it
        !! compiles before it looks in the module directories of the command.
        character(len=*), intent(in) :: dir
        character(len=HASH_LEN) :: digest, content
        character(len=512), allocatable :: files(:)
        character(len=:), allocatable :: record
        character(len=5) :: suffixes(2)
        integer :: n, i, k, ierr

        digest = ''
        record = ''
        suffixes = [character(len=5) :: '.mod', '.smod']
        allocate (files(1024))
        do k = 1, size(suffixes)
            call fs_collect_files(dir, '', trim(suffixes(k)), '', files, n, &
                recursive=.false., ierr=ierr)
            ! More module files than fit: no digest can stand for them.
            if (ierr /= 0) record = record//'unlisted '//trim(suffixes(k))//nl
            do i = 1, min(n, size(files))
                if (len_trim(files(i)) == 0) cycle
                call cache_file_digest(trim(files(i)), content)
                record = record//trim(files(i)(index(files(i), '/', back=.true.) + 1:))// &
                    ' '//content//nl
            end do
        end do
        if (len(record) > 0) digest = sha256_string(record)
    end function source_dir_modules_digest

    pure function project_relative(path, root) result(relative)
        !! path as the compiler sees it from the project root: relative when
        !! it lies below the root, unchanged otherwise.
        character(len=*), intent(in) :: path, root
        character(len=:), allocatable :: relative
        integer :: n

        n = len_trim(root)
        relative = trim(path)
        if (relative == root(:n)) then
            relative = '.'
        else if (len(relative) > n + 1) then
            if (relative(:n + 1) == root(:n)//'/') relative = relative(n + 2:)
        end if
    end function project_relative

    pure function directory_of(path) result(directory)
        !! With the trailing slash, as fx joins it to an include name.
        character(len=*), intent(in) :: path
        character(len=:), allocatable :: directory
        integer :: slash

        slash = index(path, '/', back=.true.)
        if (slash == 0) then
            directory = './'
        else
            directory = path(:slash)
        end if
    end function directory_of

    logical function exists(path)
        character(len=*), intent(in) :: path

        inquire (file=path, exist=exists)
    end function exists

end module fo_compile_inputs
