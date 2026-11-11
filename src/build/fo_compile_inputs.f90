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
    character(len=1), parameter :: nl = achar(10), tab = achar(9)

    type, public :: include_closure_t
        !! Name and content of every file the include lines resolve to, blank
        !! without includes.
        character(len=HASH_LEN) :: digest = ''
        !! The compiler reads another file than the source-tree digest holds.
        logical :: differs = .false.
        !! An include was not found without the compiler's own search list,
        !! or, with that list, cannot be keyed: the action is not cached.
        logical :: unresolved = .false.
        !! __DATE__, __TIME__ or __TIMESTAMP__ occurs: no two compiles agree.
        logical :: time_macros = .false.
    end type include_closure_t

    type :: include_line_t
        !! One file name a line makes the compiler look up.
        character(len=:), allocatable :: name
        integer :: kind = INCLUDE_NONE
        !! #include_next and its has-include form: the search goes on after the
        !! directory the including file was found in.
        logical :: next = .false.
        !! The has-include operator of #if: only whether the file is found counts.
        logical :: probe = .false.
        !! A spelling the source-tree digest of fx does not follow.
        logical :: unseen = .false.
        !! The name is not written out (a macro): the line cannot be keyed.
        logical :: opaque = .false.
    end type include_line_t

    public :: include_closure, source_dir_modules_digest, project_relative

contains

    subroutine include_closure(source, root, dirs, chain, chain_known, closure)
        !! The includes of source as gfortran resolves them. A Fortran include
        !! is searched in the directory of source and then in dirs, at any
        !! depth; `#include "x"` next to the including file, then in dirs and
        !! chain; `#include <x>` in dirs and chain; `#include_next` in chain
        !! after the directory the including file was found in. dirs are the
        !! include directories of the command in their order, chain the
        !! compiler's complete search list (relative entries below root),
        !! valid when chain_known.
        character(len=*), intent(in) :: source, root
        character(len=*), intent(in) :: dirs(:), chain(:)
        logical, intent(in) :: chain_known
        type(include_closure_t), intent(out) :: closure
        character(len=:), allocatable :: record

        record = ''
        call walk(source, '', directory_of(source), root, dirs, chain, chain_known, 0, &
            record, closure)
        if (len(record) > 0) closure%digest = sha256_string(record)
    end subroutine include_closure

    recursive subroutine walk(file, via, main_dir, root, dirs, chain, chain_known, &
            depth, record, closure)
        !! via: the search directory file was found in, blank for the source
        !! and for a file found next to its includer.
        character(len=*), intent(in) :: file, via, main_dir, root
        character(len=*), intent(in) :: dirs(:), chain(:)
        logical, intent(in) :: chain_known
        integer, intent(in) :: depth
        character(len=:), allocatable, intent(inout) :: record
        type(include_closure_t), intent(inout) :: closure
        character(len=:), allocatable :: text, found, found_via, expected
        type(include_line_t), allocatable :: lines(:)
        character(len=HASH_LEN) :: content
        integer :: ierr, pos, line_end, j, first

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
            first = verify(text(pos:line_end), ' '//tab)
            if (first > 0) first = scan(text(pos + first - 1:pos + first - 1), '#!iIcC*')
            if (first > 0) call parse_line(text(pos:line_end), lines)
            pos = line_end + 2
            if (first == 0) cycle
            do j = 1, size(lines)
                if (lines(j)%unseen) closure%differs = .true.
                if (lines(j)%opaque) then
                    closure%unresolved = .true.
                    cycle
                end if
                call resolve(file, via, depth == 0, main_dir, root, dirs, chain, &
                    chain_known, lines(j), found, found_via, expected, &
                    closure%unresolved)
                if (found /= expected) closure%differs = .true.
                if (lines(j)%probe) then
                    if (len(found) == 0) found = 'missing'
                    record = record//'has '//lines(j)%name//' '// &
                        project_relative(found, root)//nl
                    cycle
                end if
                if (len(found) == 0) then
                    record = record//'missing '//lines(j)%name//nl
                    cycle
                end if
                call cache_file_digest(found, content)
                record = record//project_relative(found, root)//' '//content//nl
                call walk(found, found_via, main_dir, root, dirs, chain, chain_known, &
                    depth + 1, record, closure)
            end do
        end do
    end subroutine walk

    subroutine resolve(file, via, primary, main_dir, root, dirs, chain, chain_known, &
            line, found, found_via, expected, unresolved)
        !! found: the file the compiler reads, found_via the search directory
        !! it lies in; expected: the one the source-tree digest of fx holds.
        !! Blank when there is none.
        character(len=*), intent(in) :: file, via, main_dir, root
        logical, intent(in) :: primary
        character(len=*), intent(in) :: dirs(:), chain(:)
        logical, intent(in) :: chain_known
        type(include_line_t), intent(in) :: line
        character(len=:), allocatable, intent(out) :: found, found_via, expected
        logical, intent(inout) :: unresolved
        character(len=:), allocatable :: candidate
        integer :: i, first

        found = ''
        found_via = ''
        expected = ''
        if (line%name(1:1) == '/') then
            if (exists(line%name)) found = line%name
            if (.not. line%unseen) expected = found
            return
        end if
        ! gfortran treats #include_next in the compiled source as #include.
        if (line%next .and. .not. primary) then
            if (.not. chain_known) then
                unresolved = .true.
                return
            end if
            first = 1
            if (len(via) > 0) then
                first = 0
                do i = 1, size(chain)
                    if (absolute(chain(i), root) /= absolute(via, root)) cycle
                    first = i + 1
                    exit
                end do
                ! Where the compiler continues is not known.
                if (first == 0) then
                    unresolved = .true.
                    return
                end if
            end if
            call search_chain(first)
            return
        end if
        do i = 1, size(dirs)
            candidate = trim(dirs(i))//'/'//line%name
            if (.not. exists(candidate)) cycle
            found = candidate
            found_via = trim(dirs(i))
            exit
        end do
        expected = found
        if (line%kind /= INCLUDE_ANGLE) then
            candidate = directory_of(file)//line%name
            if (exists(candidate)) expected = candidate
        end if
        select case (line%kind)
        case (INCLUDE_FORTRAN)
            candidate = main_dir//line%name
            if (exists(candidate)) then
                found = candidate
                found_via = ''
            end if
        case (INCLUDE_QUOTE)
            candidate = directory_of(file)//line%name
            if (exists(candidate)) then
                found = candidate
                found_via = ''
            end if
        end select
        if (line%unseen) expected = ''
        if (len(found) > 0) return
        if (.not. chain_known) unresolved = .true.
        call search_chain(1)

    contains

        subroutine search_chain(from)
            integer, intent(in) :: from
            integer :: k

            do k = from, size(chain)
                candidate = absolute(chain(k), root)//'/'//line%name
                if (.not. exists(candidate)) cycle
                found = candidate
                found_via = trim(chain(k))
                return
            end do
        end subroutine search_chain

    end subroutine resolve

    subroutine parse_line(line, lines)
        !! The file names a line makes gfortran look up: a Fortran `include`
        !! (also with blanks inside the keyword, as fixed form allows, and
        !! behind the conditional-compilation sentinel `!$`), `#include`,
        !! `#include_next`, `#import`, and the has-include operator in any
        !! other directive.
        character(len=*), intent(in) :: line
        type(include_line_t), allocatable, intent(out) :: lines(:)
        type(include_line_t) :: item
        character(len=:), allocatable :: word
        integer :: i, k

        allocate (lines(0))
        i = verify(line, ' '//tab)
        if (i == 0) return
        if (line(i:i) /= '#') then
            call fortran_include(line, i, item)
            if (item%kind /= INCLUDE_NONE) lines = [item]
            return
        end if
        i = skip_blanks(line, i + 1)
        k = i
        do while (k <= len(line))
            if (verify(line(k:k), 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ'// &
                '0123456789_') /= 0) exit
            k = k + 1
        end do
        word = lower(line(i:k - 1))
        select case (word)
        case ('include', 'include_next', 'import')
            item%next = word == 'include_next'
            item%unseen = word /= 'include'
            call operand(line, k, item)
            if (item%kind /= INCLUDE_NONE .or. item%opaque) lines = [item]
        case default
            call has_include_probes(line, k, lines)
        end select
    end subroutine parse_line

    subroutine fortran_include(line, start, item)
        !! `include 'x'` from column start, in any letter case.
        character(len=*), intent(in) :: line
        integer, intent(in) :: start
        type(include_line_t), intent(out) :: item
        character(len=*), parameter :: keyword = 'include'
        character(len=1) :: closing
        integer :: i, n, k

        i = start
        if (i + 1 <= len(line)) then
            if (line(i:i + 1) == '!$') then
                item%unseen = .true.
                i = i + 2
            else if (start == 1 .and. &
                    (line(1:2) == 'c$' .or. line(1:2) == 'C$' .or. line(1:2) == '*$')) then
                item%unseen = .true.
                i = 3
            end if
        end if
        n = 0
        do while (n < len(keyword))
            if (i > len(line)) return
            if (line(i:i) == ' ' .or. line(i:i) == tab) then
                if (n > 0) item%unseen = .true.
            else
                if (lower(line(i:i)) /= keyword(n + 1:n + 1)) return
                n = n + 1
            end if
            i = i + 1
        end do
        i = skip_blanks(line, i)
        if (i > len(line)) return
        if (line(i:i) /= '''' .and. line(i:i) /= '"') return
        closing = line(i:i)
        k = index(line(i + 1:), closing)
        if (k < 2) return
        item%name = line(i + 1:i + k - 1)
        item%kind = INCLUDE_FORTRAN
    end subroutine fortran_include

    subroutine operand(line, start, item)
        !! `"x"` or `<x>` from column start; anything else is a macro.
        character(len=*), intent(in) :: line
        integer, intent(in) :: start
        type(include_line_t), intent(inout) :: item
        character(len=1) :: closing
        integer :: i, k

        item%opaque = .true.
        i = skip_blanks(line, start)
        if (i > len(line)) return
        select case (line(i:i))
        case ('"')
            closing = '"'
        case ('<')
            closing = '>'
        case default
            return
        end select
        k = index(line(i + 1:), closing)
        if (k < 2) return
        item%opaque = .false.
        item%name = line(i + 1:i + k - 1)
        item%kind = merge(INCLUDE_ANGLE, INCLUDE_QUOTE, closing == '>')
    end subroutine operand

    subroutine has_include_probes(line, start, lines)
        !! Every has-include operator (`__has_` `include` and its `_next`
        !! form, split here so that preprocessing fo itself leaves them alone)
        !! of a directive line from column start.
        character(len=*), intent(in) :: line
        integer, intent(in) :: start
        type(include_line_t), allocatable, intent(inout) :: lines(:)
        character(len=*), parameter :: probe = '__has_'//'include'
        type(include_line_t) :: item
        integer :: p, k, j

        p = start
        do
            if (p > len(line)) return
            k = index(line(p:), probe)
            if (k == 0) return
            j = p + k - 1 + len(probe)
            item = include_line_t()
            item%probe = .true.
            item%unseen = .true.
            if (j + 4 <= len(line)) then
                if (line(j:j + 4) == '_next') then
                    item%next = .true.
                    j = j + 5
                end if
            end if
            ! The internal spelling of GCC before version 10.
            if (j + 1 <= len(line)) then
                if (line(j:j + 1) == '__') j = j + 2
            end if
            p = j
            j = skip_blanks(line, j)
            ! `defined` with the bare operator asks for no file.
            if (j > len(line)) return
            if (line(j:j) /= '(') cycle
            call operand(line, j + 1, item)
            lines = [lines, item]
            p = j + 1
        end do
    end subroutine has_include_probes

    pure integer function skip_blanks(line, start) result(i)
        character(len=*), intent(in) :: line
        integer, intent(in) :: start

        i = start
        do while (i <= len(line))
            if (line(i:i) /= ' ' .and. line(i:i) /= tab) return
            i = i + 1
        end do
    end function skip_blanks

    pure function lower(text) result(lowered)
        character(len=*), intent(in) :: text
        character(len=len(text)) :: lowered
        integer :: k

        lowered = text
        do k = 1, len(text)
            if (text(k:k) >= 'A' .and. text(k:k) <= 'Z') &
                lowered(k:k) = achar(iachar(text(k:k)) + 32)
        end do
    end function lower

    pure function absolute(directory, root) result(path)
        !! directory below root when relative, without trailing slashes.
        character(len=*), intent(in) :: directory, root
        character(len=:), allocatable :: path

        path = trim(directory)
        if (len(path) >= 2) then
            if (path(1:2) == './') path = path(3:)
        end if
        if (len(path) == 0) then
            path = trim(root)
        else if (path(1:1) /= '/') then
            path = trim(root)//'/'//path
        end if
        do while (len(path) > 1)
            if (path(len(path):len(path)) /= '/') exit
            path = path(:len(path) - 1)
        end do
    end function absolute

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
