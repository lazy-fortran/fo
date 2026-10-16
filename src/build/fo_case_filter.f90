module fo_case_filter
    !! The one test-case filter expression of `fo test -E`, the MCP test
    !! `filter` and the Gremlin `--filter` lane option, evaluated by
    !! model_select on the project model:
    !!
    !!   expr  := term { '|' term }         or
    !!   term  := factor { '&' factor }     and
    !!   factor:= '!' factor | '(' expr ')' | predicate
    !!   predicate:
    !!     name(GLOB)    whole-case-name glob
    !!     label(RE)     some label matches the CTest regex (as -L)
    !!     layer(NAME)   the case is in the layer
    !!     affected()    the Git delta of the project affects the case
    !!     failed()      the case failed in the last recorded run
    !!     WORD          the case carries the label WORD (`!slow`)
    !!
    !! Whitespace between tokens is free. Arguments run to the matching ')'
    !! (nested parentheses of a regex are kept).
    use fo_pattern, only: pattern_t, pattern_compile, pattern_compile_glob, &
        pattern_matches
    use, intrinsic :: iso_fortran_env, only: int64
    use fx_hash, only: sha256_string
    use fx_string, only: string_t
    implicit none
    private

    integer, parameter :: OP_AND = 1, OP_OR = 2, OP_NOT = 3, OP_NAME = 4, &
        OP_LABEL = 5, OP_LAYER = 6, OP_WORD = 7, OP_AFFECTED = 8, OP_FAILED = 9

    type :: node_t
        integer :: op = 0
        integer :: left = 0, right = 0
        character(len=:), allocatable :: arg
        type(pattern_t) :: pattern
    end type node_t

    type, public :: case_filter_t
        type(node_t), allocatable :: nodes(:)
        integer :: root = 0, n = 0
        logical :: uses_affected = .false., uses_failed = .false.
    end type case_filter_t

    public :: filter_parse, filter_eval, partition_parse, partition_keep

contains

    subroutine filter_parse(text, filter, ierr, message)
        !! ierr /= 0 with a message naming the offending position.
        character(len=*), intent(in) :: text
        type(case_filter_t), intent(out) :: filter
        integer, intent(out) :: ierr
        character(len=:), allocatable, intent(out) :: message
        integer :: pos

        ierr = 0
        message = ''
        allocate (filter%nodes(max(4, len(text))))
        pos = 1
        call parse_or(text, pos, filter, filter%root, ierr, message)
        if (ierr /= 0) return
        call skip(text, pos)
        if (pos <= len(text)) then
            ierr = 1
            message = 'unexpected '''//text(pos:pos)//''' at position '// &
                number(pos)//' of filter '''//text//''''
        end if
    end subroutine filter_parse

    logical function filter_eval(filter, name, labels, layer, affected, failed) &
            result(keep)
        type(case_filter_t), intent(in) :: filter
        character(len=*), intent(in) :: name, layer
        type(string_t), intent(in) :: labels(:)
        logical, intent(in) :: affected, failed

        keep = eval(filter%root)

    contains

        recursive logical function eval(k) result(value)
            integer, intent(in) :: k
            integer :: j

            associate (nd => filter%nodes(k))
                select case (nd%op)
                case (OP_AND)
                    value = eval(nd%left) .and. eval(nd%right)
                case (OP_OR)
                    value = eval(nd%left) .or. eval(nd%right)
                case (OP_NOT)
                    value = .not. eval(nd%left)
                case (OP_NAME)
                    value = pattern_matches(nd%pattern, name)
                case (OP_LAYER)
                    value = layer == nd%arg
                case (OP_AFFECTED)
                    value = affected
                case (OP_FAILED)
                    value = failed
                case default
                    value = .false.
                    do j = 1, size(labels)
                        if (nd%op == OP_LABEL) then
                            value = pattern_matches(nd%pattern, labels(j)%s)
                        else
                            value = labels(j)%s == nd%arg
                        end if
                        if (value) exit
                    end do
                end select
            end associate
        end function eval

    end function filter_eval

    subroutine partition_parse(spec, by_hash, part, parts, ierr, message)
        !! `hash:K/N` or `count:K/N` (1 <= K <= N) for partition_keep.
        character(len=*), intent(in) :: spec
        logical, intent(out) :: by_hash
        integer, intent(out) :: part, parts, ierr
        character(len=:), allocatable, intent(out) :: message
        integer :: colon, slash, ios1, ios2

        ierr = 1
        by_hash = .false.
        part = 0
        parts = 0
        message = 'invalid partition '''//spec//''': expected hash:K/N or count:K/N'
        colon = index(spec, ':')
        slash = index(spec, '/')
        if (colon < 2 .or. slash < colon + 2 .or. slash >= len(spec)) return
        if (spec(1:colon - 1) /= 'hash' .and. spec(1:colon - 1) /= 'count') return
        read (spec(colon + 1:slash - 1), *, iostat=ios1) part
        read (spec(slash + 1:), *, iostat=ios2) parts
        if (ios1 /= 0 .or. ios2 /= 0) return
        if (part < 1 .or. parts < 1 .or. part > parts) return
        by_hash = spec(1:colon - 1) == 'hash'
        ierr = 0
        message = ''
    end subroutine partition_parse

    subroutine partition_keep(by_hash, part, parts, names, keep)
        !! The cases of partition part of parts over the ordered case list
        !! names: hash assigns a case by the SHA-256 of its name alone, so a
        !! case keeps its partition when the list changes; count gives each
        !! partition a contiguous, balanced block of the list. The
        !! partitions are disjoint and together cover the list.
        logical, intent(in) :: by_hash
        integer, intent(in) :: part, parts
        type(string_t), intent(in) :: names(:)
        logical, intent(out) :: keep(size(names))
        character(len=64) :: digest
        integer(int64) :: value
        integer :: i, k, total

        total = size(names)
        do i = 1, total
            if (by_hash) then
                digest = sha256_string(names(i)%s)
                value = 0_int64
                do k = 1, 8
                    value = value*16_int64 + int(index('0123456789abcdef', &
                        digest(k:k)) - 1, int64)
                end do
                keep(i) = int(mod(value, int(parts, int64))) == part - 1
            else
                keep(i) = (i - 1)*parts/max(1, total) + 1 == part
            end if
        end do
    end subroutine partition_keep

    recursive subroutine parse_or(text, pos, filter, node, ierr, message)
        character(len=*), intent(in) :: text
        integer, intent(inout) :: pos
        type(case_filter_t), intent(inout) :: filter
        integer, intent(out) :: node, ierr
        character(len=:), allocatable, intent(inout) :: message
        integer :: right

        call parse_and(text, pos, filter, node, ierr, message)
        do while (ierr == 0)
            call skip(text, pos)
            if (pos > len(text)) return
            if (text(pos:pos) /= '|') return
            pos = pos + 1
            call parse_and(text, pos, filter, right, ierr, message)
            if (ierr /= 0) return
            node = add(filter, OP_OR, node, right)
        end do
    end subroutine parse_or

    recursive subroutine parse_and(text, pos, filter, node, ierr, message)
        character(len=*), intent(in) :: text
        integer, intent(inout) :: pos
        type(case_filter_t), intent(inout) :: filter
        integer, intent(out) :: node, ierr
        character(len=:), allocatable, intent(inout) :: message
        integer :: right

        call parse_factor(text, pos, filter, node, ierr, message)
        do while (ierr == 0)
            call skip(text, pos)
            if (pos > len(text)) return
            if (text(pos:pos) /= '&') return
            pos = pos + 1
            call parse_factor(text, pos, filter, right, ierr, message)
            if (ierr /= 0) return
            node = add(filter, OP_AND, node, right)
        end do
    end subroutine parse_and

    recursive subroutine parse_factor(text, pos, filter, node, ierr, message)
        character(len=*), intent(in) :: text
        integer, intent(inout) :: pos
        type(case_filter_t), intent(inout) :: filter
        integer, intent(out) :: node, ierr
        character(len=:), allocatable, intent(inout) :: message
        character(len=:), allocatable :: word, arg
        integer :: start, depth, inner, code
        character(len=:), allocatable :: regex_message

        node = 0
        ierr = 0
        call skip(text, pos)
        if (pos > len(text)) then
            ierr = 1
            message = 'filter '''//text//''' ends where a predicate is expected'
            return
        end if
        select case (text(pos:pos))
        case ('!')
            pos = pos + 1
            call parse_factor(text, pos, filter, inner, ierr, message)
            if (ierr == 0) node = add(filter, OP_NOT, inner, 0)
            return
        case ('(')
            pos = pos + 1
            call parse_or(text, pos, filter, node, ierr, message)
            if (ierr /= 0) return
            call skip(text, pos)
            if (pos > len(text)) then
                ierr = 1
                message = 'missing '')'' in filter '''//text//''''
            else if (text(pos:pos) /= ')') then
                ierr = 1
                message = 'expected '')'' at position '//number(pos)// &
                    ' of filter '''//text//''''
            else
                pos = pos + 1
            end if
            return
        end select
        start = pos
        do while (pos <= len(text))
            if (index(' ()&|!', text(pos:pos)) > 0) exit
            pos = pos + 1
        end do
        if (pos == start) then
            ierr = 1
            message = 'unexpected '''//text(pos:pos)//''' at position '// &
                number(pos)//' of filter '''//text//''''
            return
        end if
        word = text(start:pos - 1)
        call skip(text, pos)
        if (pos > len(text)) then
            node = bare(word)
            return
        end if
        if (text(pos:pos) /= '(') then
            node = bare(word)
            return
        end if
        pos = pos + 1
        start = pos
        depth = 1
        do while (pos <= len(text))
            if (text(pos:pos) == '(') depth = depth + 1
            if (text(pos:pos) == ')') depth = depth - 1
            if (depth == 0) exit
            pos = pos + 1
        end do
        if (depth /= 0) then
            ierr = 1
            message = 'missing '')'' after '//word//'( in filter '''//text//''''
            return
        end if
        arg = adjustl(text(start:pos - 1))
        arg = trim(arg)
        pos = pos + 1
        select case (word)
        case ('name')
            node = add(filter, OP_NAME, 0, 0, arg)
            call pattern_compile_glob(arg, filter%nodes(node)%pattern)
        case ('label')
            node = add(filter, OP_LABEL, 0, 0, arg)
            call pattern_compile(arg, filter%nodes(node)%pattern, code, regex_message)
            if (code /= 0) then
                ierr = 1
                message = 'invalid regex in label('//arg//'): '//regex_message
            end if
        case ('layer')
            node = add(filter, OP_LAYER, 0, 0, arg)
        case ('affected')
            node = add(filter, OP_AFFECTED, 0, 0)
            filter%uses_affected = .true.
            if (len(arg) > 0) call no_argument()
        case ('failed')
            node = add(filter, OP_FAILED, 0, 0)
            filter%uses_failed = .true.
            if (len(arg) > 0) call no_argument()
        case default
            ierr = 1
            message = 'unknown filter predicate '//word//'() in '''//text//''''
        end select
        if (ierr == 0 .and. len(arg) == 0 .and. node > 0) then
            if (filter%nodes(node)%op == OP_NAME .or. filter%nodes(node)%op == &
                OP_LABEL .or. filter%nodes(node)%op == OP_LAYER) then
                ierr = 1
                message = word//'() needs an argument in '''//text//''''
            end if
        end if

    contains

        subroutine no_argument()
            ierr = 1
            message = word//'() takes no argument in '''//text//''''
        end subroutine no_argument

        integer function bare(label)
            character(len=*), intent(in) :: label

            bare = add(filter, OP_WORD, 0, 0, label)
        end function bare

    end subroutine parse_factor

    integer function add(filter, op, left, right, arg) result(k)
        type(case_filter_t), intent(inout) :: filter
        integer, intent(in) :: op, left, right
        character(len=*), intent(in), optional :: arg
        type(node_t), allocatable :: grown(:)

        if (filter%n >= size(filter%nodes)) then
            allocate (grown(2*size(filter%nodes)))
            grown(1:filter%n) = filter%nodes(1:filter%n)
            call move_alloc(grown, filter%nodes)
        end if
        filter%n = filter%n + 1
        k = filter%n
        filter%nodes(k)%op = op
        filter%nodes(k)%left = left
        filter%nodes(k)%right = right
        filter%nodes(k)%arg = ''
        if (present(arg)) filter%nodes(k)%arg = arg
    end function add

    subroutine skip(text, pos)
        character(len=*), intent(in) :: text
        integer, intent(inout) :: pos

        do while (pos <= len(text))
            if (text(pos:pos) /= ' ' .and. text(pos:pos) /= achar(9)) exit
            pos = pos + 1
        end do
    end subroutine skip

    function number(i) result(text)
        integer, intent(in) :: i
        character(len=:), allocatable :: text
        character(len=16) :: buffer

        write (buffer, '(i0)') i
        text = trim(buffer)
    end function number

end module fo_case_filter
