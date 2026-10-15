module fo_pattern
    !! Name and label patterns used by test selection. Selection semantics live
    !! only in fo (fo_project_model); backends receive exact case names.
    !!
    !! Regular expressions follow CMake string(REGEX) / CTest -L syntax, so
    !! `fo test -L RE` selects the labels `ctest -L RE` would on every backend:
    !!   ^  $       beginning / end of the input (anywhere in the pattern)
    !!   .          any single character
    !!   \c         the literal character c (\. \\ \a ...; no \d \w \s classes)
    !!   [set]      one character of set; [^set] one character not in set;
    !!              a-z ranges; ] first is a member, - first or last literal,
    !!              \ inside brackets is an ordinary member
    !!   * + ?      zero-or-more / one-or-more / zero-or-one of the previous atom
    !!   a|b        alternation, lower precedence than concatenation
    !!   ( )        grouping
    !! Matching is unanchored search, as in CTest. Not supported (as in CMake):
    !! {m,n} counts, back references, POSIX [:class:] names, non-greedy forms.
    !! Difference: CMake rejects a repeated operand that can match empty, e.g.
    !! (a*)*; fo accepts it with the obvious meaning.
    !!
    !! Globs (whole-name, for case names): * any run, ? one character,
    !! [abc] [a-z] [!abc] one character of a set; an unterminated [ is literal.
    !!
    !! Patterns compile once (pattern_t) to a small instruction list that a
    !! Pike VM runs in O(len(pattern) * len(text)) without backtracking.
    implicit none
    private

    public :: pattern_t, pattern_compile, pattern_compile_glob, pattern_matches
    public :: pattern_search, pattern_glob, pattern_is_glob, pattern_check

    integer, parameter :: OP_CHAR = 1
    integer, parameter :: OP_ANY = 2
    integer, parameter :: OP_CLASS = 3
    integer, parameter :: OP_SPLIT = 4
    integer, parameter :: OP_JMP = 5
    integer, parameter :: OP_BOL = 6
    integer, parameter :: OP_EOL = 7
    integer, parameter :: OP_MATCH = 8

    type :: inst_t
        integer :: op = 0
        !! Relative jump offsets of SPLIT/JMP, so fragments concatenate freely.
        integer :: x = 0
        integer :: y = 0
        character :: c = ' '
        logical, allocatable :: set(:)
    end type inst_t

    type :: pattern_t
        !! A compiled regular expression or glob.
        private
        type(inst_t), allocatable :: code(:)
    end type pattern_t

contains

    subroutine pattern_compile(regex, pattern, ierr, message)
        !! Compile a CTest regular expression.
        character(len=*), intent(in) :: regex
        type(pattern_t), intent(out) :: pattern
        integer, intent(out) :: ierr
        character(len=:), allocatable, intent(out) :: message

        call compile_regex(regex, pattern%code, ierr, message)
    end subroutine pattern_compile

    subroutine pattern_compile_glob(glob, pattern)
        !! Compile a whole-name glob; every glob is valid.
        character(len=*), intent(in) :: glob
        type(pattern_t), intent(out) :: pattern
        integer :: ierr
        character(len=:), allocatable :: message

        call compile_regex(glob_regex(glob), pattern%code, ierr, message)
    end subroutine pattern_compile_glob

    logical function pattern_matches(pattern, text) result(found)
        !! Regex: matches anywhere in text. Glob: matches all of text.
        type(pattern_t), intent(in) :: pattern
        character(len=*), intent(in) :: text

        found = .false.
        if (.not. allocated(pattern%code)) return
        found = run_code(pattern%code, text)
    end function pattern_matches

    subroutine pattern_check(regex, ierr, message)
        !! Validate a CTest regular expression without matching.
        character(len=*), intent(in) :: regex
        integer, intent(out) :: ierr
        character(len=:), allocatable, intent(out) :: message
        type(pattern_t) :: pattern

        call pattern_compile(regex, pattern, ierr, message)
    end subroutine pattern_check

    logical function pattern_search(regex, text, ierr) result(found)
        !! True when the CTest regular expression matches anywhere in text.
        character(len=*), intent(in) :: regex, text
        integer, intent(out) :: ierr
        type(pattern_t) :: pattern
        character(len=:), allocatable :: message

        found = .false.
        call pattern_compile(regex, pattern, ierr, message)
        if (ierr /= 0) return
        found = pattern_matches(pattern, text)
    end function pattern_search

    logical function pattern_is_glob(name) result(is_glob)
        !! A name containing *, ? or [ is a glob candidate.
        character(len=*), intent(in) :: name

        is_glob = scan(name, '*?[') > 0
    end function pattern_is_glob

    logical function pattern_glob(glob, name) result(matched)
        !! Whole-name shell glob match.
        character(len=*), intent(in) :: glob, name
        type(pattern_t) :: pattern

        call pattern_compile_glob(glob, pattern)
        matched = pattern_matches(pattern, name)
    end function pattern_glob

    function glob_regex(glob) result(regex)
        !! Anchored regular expression equivalent to a glob.
        character(len=*), intent(in) :: glob
        character(len=:), allocatable :: regex
        integer :: i, close_at

        regex = '^'
        i = 1
        do while (i <= len(glob))
            select case (glob(i:i))
            case ('*')
                regex = regex//'.*'
            case ('?')
                regex = regex//'.'
            case ('[')
                close_at = bracket_end(glob, i)
                if (close_at == 0) then
                    regex = regex//'\['
                else
                    if (glob(i + 1:i + 1) == '!') then
                        regex = regex//'[^'//glob(i + 2:close_at)
                    else
                        regex = regex//glob(i:close_at)
                    end if
                    i = close_at
                end if
            case default
                regex = regex//'\'//glob(i:i)
            end select
            i = i + 1
        end do
        regex = regex//'$'
    end function glob_regex

    integer function bracket_end(glob, open_at) result(close_at)
        !! Index of the ] closing the glob set opened at open_at, else 0.
        character(len=*), intent(in) :: glob
        integer, intent(in) :: open_at
        integer :: k

        close_at = 0
        k = open_at + 1
        if (k <= len(glob)) then
            if (glob(k:k) == '!') k = k + 1
        end if
        ! A ] right after [ or [! is a member, not the terminator.
        if (k <= len(glob)) then
            if (glob(k:k) == ']') k = k + 1
        end if
        do while (k <= len(glob))
            if (glob(k:k) == ']') then
                close_at = k
                return
            end if
            k = k + 1
        end do
    end function bracket_end

    subroutine compile_regex(pattern, code, ierr, message)
        character(len=*), intent(in) :: pattern
        type(inst_t), allocatable, intent(out) :: code(:)
        integer, intent(out) :: ierr
        character(len=:), allocatable, intent(out) :: message
        integer :: pos

        ierr = 0
        message = ''
        pos = 1
        call parse_alt(pattern, pos, code, ierr, message)
        if (ierr /= 0) return
        if (pos <= len(pattern)) then
            ierr = 1
            message = 'unmatched ) in regular expression: '//pattern
            return
        end if
        call push(code, inst_t(op=OP_MATCH))
    end subroutine compile_regex

    recursive subroutine parse_alt(p, pos, code, ierr, message)
        character(len=*), intent(in) :: p
        integer, intent(inout) :: pos
        type(inst_t), allocatable, intent(out) :: code(:)
        integer, intent(inout) :: ierr
        character(len=:), allocatable, intent(inout) :: message
        type(inst_t), allocatable :: rhs(:)

        call parse_seq(p, pos, code, ierr, message)
        do
            if (ierr /= 0) return
            if (pos > len(p)) return
            if (p(pos:pos) /= '|') return
            pos = pos + 1
            call parse_seq(p, pos, rhs, ierr, message)
            if (ierr /= 0) return
            call prepend(code, inst_t(op=OP_SPLIT, x=1, y=size(code) + 2))
            call push(code, inst_t(op=OP_JMP, x=size(rhs) + 1))
            call append_all(code, rhs)
        end do
    end subroutine parse_alt

    recursive subroutine parse_seq(p, pos, code, ierr, message)
        character(len=*), intent(in) :: p
        integer, intent(inout) :: pos
        type(inst_t), allocatable, intent(out) :: code(:)
        integer, intent(inout) :: ierr
        character(len=:), allocatable, intent(inout) :: message
        type(inst_t), allocatable :: atom(:)
        integer :: n

        allocate(code(0))
        do while (pos <= len(p))
            select case (p(pos:pos))
            case ('|', ')')
                return
            case ('*', '+', '?')
                ierr = 1
                message = 'repetition without operand in regular expression: '//p
                return
            end select
            call parse_atom(p, pos, atom, ierr, message)
            if (ierr /= 0) return
            do while (pos <= len(p))
                n = size(atom)
                select case (p(pos:pos))
                case ('*')
                    call prepend(atom, inst_t(op=OP_SPLIT, x=1, y=n + 2))
                    call push(atom, inst_t(op=OP_JMP, x=-(n + 1)))
                case ('+')
                    call push(atom, inst_t(op=OP_SPLIT, x=-n, y=1))
                case ('?')
                    call prepend(atom, inst_t(op=OP_SPLIT, x=1, y=n + 1))
                case default
                    exit
                end select
                pos = pos + 1
            end do
            call append_all(code, atom)
        end do
    end subroutine parse_seq

    recursive subroutine parse_atom(p, pos, code, ierr, message)
        character(len=*), intent(in) :: p
        integer, intent(inout) :: pos
        type(inst_t), allocatable, intent(out) :: code(:)
        integer, intent(inout) :: ierr
        character(len=:), allocatable, intent(inout) :: message
        type(inst_t) :: one

        select case (p(pos:pos))
        case ('(')
            pos = pos + 1
            call parse_alt(p, pos, code, ierr, message)
            if (ierr /= 0) return
            if (pos > len(p)) then
                ierr = 1
                message = 'unmatched ( in regular expression: '//p
                return
            end if
            pos = pos + 1
            return
        case ('[')
            call parse_class(p, pos, one, ierr, message)
            if (ierr /= 0) return
        case ('.')
            one%op = OP_ANY
            pos = pos + 1
        case ('^')
            one%op = OP_BOL
            pos = pos + 1
        case ('$')
            one%op = OP_EOL
            pos = pos + 1
        case ('\')
            if (pos == len(p)) then
                ierr = 1
                message = 'trailing \ in regular expression: '//p
                return
            end if
            one%op = OP_CHAR
            one%c = p(pos + 1:pos + 1)
            pos = pos + 2
        case default
            one%op = OP_CHAR
            one%c = p(pos:pos)
            pos = pos + 1
        end select
        allocate(code(1))
        code(1) = one
    end subroutine parse_atom

    subroutine parse_class(p, pos, one, ierr, message)
        !! [set] / [^set]; ] first is a member, - first or last is literal and
        !! a backslash is an ordinary member, as in CMake.
        character(len=*), intent(in) :: p
        integer, intent(inout) :: pos
        type(inst_t), intent(out) :: one
        integer, intent(inout) :: ierr
        character(len=:), allocatable, intent(inout) :: message
        logical :: negate, first
        integer :: lo, hi

        one%op = OP_CLASS
        allocate(one%set(0:255))
        one%set = .false.
        pos = pos + 1
        negate = .false.
        if (pos <= len(p)) then
            if (p(pos:pos) == '^') then
                negate = .true.
                pos = pos + 1
            end if
        end if
        first = .true.
        do
            if (pos > len(p)) then
                ierr = 1
                message = 'unterminated [ in regular expression: '//p
                return
            end if
            if (p(pos:pos) == ']' .and. .not. first) exit
            first = .false.
            lo = code_of(p(pos:pos))
            hi = lo
            if (pos + 2 <= len(p)) then
                if (p(pos + 1:pos + 1) == '-' .and. p(pos + 2:pos + 2) /= ']') then
                    hi = code_of(p(pos + 2:pos + 2))
                    pos = pos + 2
                end if
            end if
            if (hi >= lo) one%set(lo:hi) = .true.
            pos = pos + 1
        end do
        pos = pos + 1
        if (negate) one%set = .not. one%set
    end subroutine parse_class

    subroutine push(code, one)
        type(inst_t), allocatable, intent(inout) :: code(:)
        type(inst_t), intent(in) :: one
        type(inst_t), allocatable :: grown(:)
        integer :: n

        n = size(code)
        allocate(grown(n + 1))
        grown(1:n) = code
        grown(n + 1) = one
        call move_alloc(grown, code)
    end subroutine push

    subroutine prepend(code, one)
        type(inst_t), allocatable, intent(inout) :: code(:)
        type(inst_t), intent(in) :: one
        type(inst_t), allocatable :: grown(:)
        integer :: n

        n = size(code)
        allocate(grown(n + 1))
        grown(1) = one
        grown(2:n + 1) = code
        call move_alloc(grown, code)
    end subroutine prepend

    subroutine append_all(code, tail)
        type(inst_t), allocatable, intent(inout) :: code(:)
        type(inst_t), intent(in) :: tail(:)
        type(inst_t), allocatable :: grown(:)
        integer :: n

        n = size(code)
        allocate(grown(n + size(tail)))
        grown(1:n) = code
        grown(n + 1:) = tail
        call move_alloc(grown, code)
    end subroutine append_all

    integer function code_of(c) result(k)
        character, intent(in) :: c

        k = modulo(ichar(c), 256)
    end function code_of

    logical function run_code(code, text) result(found)
        !! Pike VM search: a thread starts at every text position.
        type(inst_t), intent(in) :: code(:)
        character(len=*), intent(in) :: text
        integer, allocatable :: clist(:), nlist(:), mark(:)
        integer :: ncl, nnl, i, k, pc, n

        n = size(code)
        allocate(clist(n), nlist(n), mark(n))
        mark = 0
        ncl = 0
        found = .false.
        do i = 1, len(text) + 1
            call add_thread(clist, ncl, 1, i)
            nnl = 0
            do k = 1, ncl
                pc = clist(k)
                select case (code(pc)%op)
                case (OP_MATCH)
                    found = .true.
                    return
                case (OP_ANY)
                    if (i <= len(text)) call add_thread(nlist, nnl, pc + 1, i + 1)
                case (OP_CHAR)
                    if (i <= len(text)) then
                        if (text(i:i) == code(pc)%c) &
                            call add_thread(nlist, nnl, pc + 1, i + 1)
                    end if
                case (OP_CLASS)
                    if (i <= len(text)) then
                        if (code(pc)%set(code_of(text(i:i)))) &
                            call add_thread(nlist, nnl, pc + 1, i + 1)
                    end if
                end select
            end do
            clist(1:nnl) = nlist(1:nnl)
            ncl = nnl
        end do

    contains

        recursive subroutine add_thread(list, count, at, position)
            integer, intent(inout) :: list(:)
            integer, intent(inout) :: count
            integer, intent(in) :: at, position

            if (mark(at) == position) return
            mark(at) = position
            select case (code(at)%op)
            case (OP_JMP)
                call add_thread(list, count, at + code(at)%x, position)
            case (OP_SPLIT)
                call add_thread(list, count, at + code(at)%x, position)
                call add_thread(list, count, at + code(at)%y, position)
            case (OP_BOL)
                if (position == 1) call add_thread(list, count, at + 1, position)
            case (OP_EOL)
                if (position == len(text) + 1) &
                    call add_thread(list, count, at + 1, position)
            case default
                count = count + 1
                list(count) = at
            end select
        end subroutine add_thread

    end function run_code

end module fo_pattern
