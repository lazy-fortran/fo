module fo_cmake_delta
    !! Impact of an edit to a CMake build description (#197, #189).
    !!
    !! An edit is a test registration when the new file equals the old one
    !! except for commands it inserts and literal foreach item lists it
    !! extends, and every command those insertions and new loop iterations
    !! execute only creates targets or tests (add_executable, add_library,
    !! add_test) or configures one it creates in the same edit (target_*,
    !! set_target_properties, set_tests_properties, set_property on TARGET or
    !! TEST, add_dependencies, and project commands the caller declares).
    !! Such an edit cannot change an existing target or test except through
    !! configure-time enumeration of all targets or tests, whose consumers
    !! callers keep (model_affected). Every other edit, including any edit
    !! inside a function or macro body, a removed command, a changed
    !! argument, a fixture setup or cleanup property or an unparsable file,
    !! is not a registration and widens the selection.
    !!
    !! Tests and targets are returned as name templates in which '*' stands
    !! for any text: a superset of the names the edit creates.
    use, intrinsic :: iso_fortran_env, only: int64
    use fx_string, only: string_t, to_lower
    implicit none
    private

    public :: cmake_registration_delta, cmake_template_matches

    type, public :: cmake_delta_t
        !! Changed build descriptions (relative to the project root) whose
        !! edits only register tests and targets, and the name templates of
        !! the tests and targets those edits create.
        type(string_t), allocatable :: paths(:), tests(:), targets(:)
    end type cmake_delta_t

    integer, parameter :: KEPT = 0, INSERTED = 1, MODIFIED = 2
    integer, parameter :: KIND_TEST = 1, KIND_TARGET = 2
    !! Largest middle section (old x new commands) the alignment compares.
    integer, parameter :: MAX_CELLS = 25000000

    type :: command_t
        character(len=:), allocatable :: name, key
        type(string_t), allocatable :: args(:)
    end type command_t

    type :: creation_t
        character(len=:), allocatable :: template
        integer :: kind = 0, command = 0, loop = 0
    end type creation_t

contains

    subroutine cmake_registration_delta(old_text, new_text, commands, tests, &
            targets, ok, reason, context)
        !! ok: the edit from old_text to new_text only registers tests and
        !! targets; tests and targets hold their name templates. Otherwise
        !! reason says which command prevents it. commands are project
        !! commands whose first argument names a test the edit creates and
        !! which change only that test. context holds the other CMake files
        !! of the project, whose commands must not name what the edit creates.
        character(len=*), intent(in) :: old_text, new_text
        type(string_t), intent(in) :: commands(:)
        type(string_t), allocatable, intent(out) :: tests(:), targets(:)
        logical, intent(out) :: ok
        character(len=:), allocatable, intent(out) :: reason
        type(string_t), intent(in), optional :: context(:)
        type(command_t), allocatable :: old(:), new(:)
        type(string_t), allocatable :: added(:)
        type(creation_t), allocatable :: made(:)
        integer, allocatable :: status(:), offsets(:), stack(:)
        logical, allocatable :: fresh(:), wild(:)
        type(command_t), allocatable :: others(:)
        integer, allocatable :: inner(:)
        integer :: j, k, top, opener, item
        logical :: parsed

        allocate(tests(0), targets(0), made(0), added(0), offsets(0))
        ok = .false.
        reason = ''
        call parse_commands(old_text, old, parsed)
        if (parsed) call parse_commands(new_text, new, parsed)
        if (.not. parsed) then
            reason = 'cannot parse the CMake commands'
            return
        end if
        call align(old, new, status, added, offsets, reason)
        if (len(reason) > 0) return

        ! Context of each new command: commands inserted or executed by an
        ! inserted or extended loop are fresh; wild ones have no old instance.
        allocate(fresh(size(new)), wild(size(new)), inner(size(new)), &
            stack(size(new)))
        top = 0
        do j = 1, size(new)
            fresh(j) = .false.
            wild(j) = .false.
            inner(j) = 0
            select case (new(j)%name)
            case ('endforeach', 'endwhile', 'endif', 'endfunction', 'endmacro', &
                    'endblock')
                ! A closer belongs to its opener; equal closers align anywhere.
                if (top == 0) then
                    reason = 'unbalanced '//new(j)%name
                    return
                end if
                top = top - 1
                cycle
            end select
            fresh(j) = status(j) /= KEPT
            wild(j) = status(j) == INSERTED
            do k = 1, top
                if (new(stack(k))%name /= 'foreach') cycle
                inner(j) = stack(k)
                if (status(stack(k)) /= KEPT) fresh(j) = .true.
                if (status(stack(k)) == INSERTED) wild(j) = .true.
            end do
            if (fresh(j)) then
                do k = 1, top
                    select case (new(stack(k))%name)
                    case ('function', 'macro')
                        reason = 'edit inside a function or macro body: '//new(j)%name
                        return
                    end select
                end do
            end if
            select case (new(j)%name)
            case ('foreach', 'while', 'if', 'function', 'macro', 'block')
                top = top + 1
                stack(top) = j
            end select
            if (.not. fresh(j)) cycle
            select case (new(j)%name)
            case ('add_executable', 'add_library', 'add_test')
                call creation(j)
                if (len(reason) > 0) return
            end select
        end do
        if (top /= 0) then
            reason = 'unbalanced CMake blocks'
            return
        end if

        do j = 1, size(new)
            if (.not. fresh(j)) cycle
            select case (new(j)%name)
            case ('foreach', 'add_executable', 'add_library', 'add_test')
                cycle
            end select
            call configure(j)
            if (len(reason) > 0) return
        end do

        do k = 1, size(made)
            j = made(k)%command
            if (wild(j)) then
                call emit(made(k)%kind, wildcard(made(k)%template))
                cycle
            end if
            ! Only extended loops make this unchanged command fresh: its new
            ! instances take a new item of one of them.
            do opener = 1, j - 1
                if (status(opener) /= MODIFIED) cycle
                if (.not. encloses(opener, j)) cycle
                do item = offsets(opener) + 1, next_offset(opener)
                    call emit(made(k)%kind, wildcard(substitute(made(k)%template, &
                        new(opener)%args(1)%s, added(item)%s)))
                end do
            end do
        end do

        ! Unchanged commands that name a created target or test would now
        ! see it: if(TARGET ...), a link item that becomes this target, ...
        do j = 1, size(new)
            if (fresh(j)) cycle
            call unseen(new(j))
            if (len(reason) > 0) return
        end do
        if (present(context)) then
            do k = 1, size(context)
                call parse_commands(context(k)%s, others, parsed)
                if (.not. parsed) then
                    reason = 'cannot parse another CMake file of the project'
                    return
                end if
                do j = 1, size(others)
                    call unseen(others(j))
                    if (len(reason) > 0) return
                end do
            end do
        end if
        ok = .true.

    contains

        subroutine creation(j)
            integer, intent(in) :: j
            type(creation_t) :: c

            if (size(new(j)%args) == 0) then
                reason = new(j)%name//' without arguments'
                return
            end if
            c%template = new(j)%args(1)%s
            c%kind = KIND_TARGET
            if (new(j)%name == 'add_test') then
                c%kind = KIND_TEST
                if (new(j)%args(1)%s == 'NAME') then
                    if (size(new(j)%args) < 2) then
                        reason = 'add_test without a name'
                        return
                    end if
                    c%template = new(j)%args(2)%s
                end if
            end if
            c%command = j
            c%loop = inner(j)
            made = [made, c]
        end subroutine creation

        subroutine configure(j)
            !! A fresh configuring command must name only creations of the edit.
            integer, intent(in) :: j
            integer :: a, first, last, kind

            associate (args => new(j)%args, name => new(j)%name)
                if (size(args) == 0) then
                    reason = name//' changes more than test registrations'
                    return
                end if
                first = 1
                last = 1
                kind = KIND_TARGET
                select case (name)
                case ('target_link_libraries', 'target_include_directories', &
                        'target_compile_options', 'target_compile_definitions', &
                        'target_compile_features', 'target_link_options', &
                        'target_link_directories', 'target_sources', &
                        'target_precompile_headers', 'add_dependencies')
                case ('set_target_properties', 'set_tests_properties')
                    if (name == 'set_tests_properties') kind = KIND_TEST
                    last = keyword_at(args, 'PROPERTIES') - 1
                case ('set_property')
                    first = 2
                    if (args(1)%s == 'TEST') then
                        kind = KIND_TEST
                    else if (args(1)%s /= 'TARGET') then
                        reason = 'set_property('//args(1)%s// &
                            ') changes more than test registrations'
                        return
                    end if
                    last = size(args)
                    do a = 2, size(args)
                        select case (args(a)%s)
                        case ('APPEND', 'APPEND_STRING', 'PROPERTY', 'DIRECTORY')
                            last = a - 1
                            exit
                        end select
                    end do
                    if (keyword_at(args, 'DIRECTORY') <= size(args)) then
                        reason = 'set_property in another directory'
                        return
                    end if
                case default
                    if (.not. any_named(commands, name)) then
                        reason = name//' changes more than test registrations'
                        return
                    end if
                    kind = KIND_TEST
                end select
                if (kind == KIND_TEST) then
                    do a = 1, size(args)
                        select case (args(a)%s)
                        case ('FIXTURES_SETUP', 'FIXTURES_CLEANUP')
                            reason = 'a new test sets up or cleans a fixture'
                            return
                        end select
                    end do
                end if
                if (last < first) then
                    reason = name//' names nothing'
                    return
                end if
                do a = first, last
                    if (.not. created(args(a)%s, kind, inner(j))) then
                        reason = name//' changes '//args(a)%s// &
                            ', which this edit does not create'
                        return
                    end if
                end do
            end associate
        end subroutine configure

        logical function created(subject, kind, loop) result(found)
            character(len=*), intent(in) :: subject
            integer, intent(in) :: kind, loop
            integer :: k
            logical :: variable

            variable = index(subject, '$') > 0
            found = .false.
            do k = 1, size(made)
                if (made(k)%kind /= kind) cycle
                if (made(k)%template /= subject) cycle
                ! A variable name means one thing only within one iteration.
                if (variable .and. made(k)%loop /= loop) cycle
                found = .true.
                return
            end do
        end function created

        subroutine unseen(command)
            type(command_t), intent(in) :: command
            integer :: a, k

            do a = 1, size(command%args)
                if (index(command%args(a)%s, '$') > 0) cycle
                do k = 1, size(tests)
                    if (cmake_template_matches(tests(k)%s, command%args(a)%s)) &
                        reason = 'an unchanged '//command%name//' names the new test '// &
                            command%args(a)%s
                end do
                do k = 1, size(targets)
                    if (cmake_template_matches(targets(k)%s, command%args(a)%s)) &
                        reason = 'an unchanged '//command%name//' names the new target '// &
                            command%args(a)%s
                end do
                if (len(reason) > 0) return
            end do
        end subroutine unseen

        logical function encloses(opener, j) result(inside)
            integer, intent(in) :: opener, j
            integer :: k, depth

            inside = .false.
            if (opener >= j) return
            depth = 0
            do k = opener + 1, j - 1
                select case (new(k)%name)
                case ('foreach', 'while', 'if', 'function', 'macro', 'block')
                    depth = depth + 1
                case ('endforeach', 'endwhile', 'endif', 'endfunction', 'endmacro', &
                        'endblock')
                    depth = depth - 1
                    if (depth < 0) return
                end select
            end do
            inside = .true.
        end function encloses

        integer function next_offset(opener) result(last)
            integer, intent(in) :: opener
            integer :: k

            last = size(added)
            do k = opener + 1, size(new)
                if (status(k) /= MODIFIED) cycle
                last = offsets(k)
                return
            end do
        end function next_offset

        subroutine emit(kind, template)
            integer, intent(in) :: kind
            character(len=*), intent(in) :: template

            if (kind == KIND_TEST) then
                tests = [tests, string_t(template)]
            else
                targets = [targets, string_t(template)]
            end if
        end subroutine emit

    end subroutine cmake_registration_delta

    subroutine align(old, new, status, added, offsets, reason)
        !! status(j) of each new command: KEPT, INSERTED or MODIFIED (a
        !! literal foreach whose old items are a subsequence of its new items;
        !! its new items are added(offsets(j)+1:) up to the next MODIFIED).
        !! reason is set when an old command is removed or changed otherwise.
        type(command_t), intent(in) :: old(:), new(:)
        integer, allocatable, intent(out) :: status(:), offsets(:)
        type(string_t), allocatable, intent(inout) :: added(:)
        character(len=:), allocatable, intent(inout) :: reason
        integer, allocatable :: lcs(:, :), old_hash(:), new_hash(:), gap_old(:), gap_new(:)
        integer :: m, n, p, s, mo, mn, a, b, n_old, n_new

        m = size(old)
        n = size(new)
        allocate(status(n), offsets(n), gap_old(m), gap_new(n))
        status = INSERTED
        offsets = 0
        old_hash = [(key_hash(old(a)%key), a=1, m)]
        new_hash = [(key_hash(new(b)%key), b=1, n)]
        p = 0
        do while (p < min(m, n))
            if (.not. same(p + 1, p + 1)) exit
            p = p + 1
        end do
        s = 0
        do while (s < min(m, n) - p)
            if (.not. same(m - s, n - s)) exit
            s = s + 1
        end do
        status(1:p) = KEPT
        status(n - s + 1:n) = KEPT
        mo = m - p - s
        mn = n - p - s
        if (real(mo + 1) * real(mn + 1) > real(MAX_CELLS)) then
            reason = 'edit too large to align'
            return
        end if
        allocate(lcs(0:mo, 0:mn))
        lcs = 0
        do a = mo - 1, 0, -1
            do b = mn - 1, 0, -1
                if (same(p + a + 1, p + b + 1)) then
                    lcs(a, b) = lcs(a + 1, b + 1) + 1
                else
                    lcs(a, b) = max(lcs(a + 1, b), lcs(a, b + 1))
                end if
            end do
        end do
        a = 0
        b = 0
        n_old = 0
        n_new = 0
        do while (a < mo .or. b < mn)
            if (a < mo .and. b < mn) then
                if (same(p + a + 1, p + b + 1)) then
                    call close_gap()
                    if (len(reason) > 0) return
                    status(p + b + 1) = KEPT
                    a = a + 1
                    b = b + 1
                    cycle
                end if
            end if
            if (b < mn) then
                if (a == mo) then
                    n_new = n_new + 1
                    gap_new(n_new) = p + b + 1
                    b = b + 1
                    cycle
                end if
                if (lcs(a, b + 1) >= lcs(a + 1, b)) then
                    n_new = n_new + 1
                    gap_new(n_new) = p + b + 1
                    b = b + 1
                    cycle
                end if
            end if
            n_old = n_old + 1
            gap_old(n_old) = p + a + 1
            a = a + 1
        end do
        call close_gap()

    contains

        logical function same(i, j)
            integer, intent(in) :: i, j

            same = old_hash(i) == new_hash(j)
            if (same) same = old(i)%key == new(j)%key
        end function same

        subroutine close_gap()
            !! Each removed old command must be a literal foreach that a new
            !! foreach of the same gap extends.
            integer :: d, i, from
            type(string_t), allocatable :: old_items(:), new_items(:), extra(:)
            logical :: literal

            from = 1
            do d = 1, n_old
                associate (o => old(gap_old(d)))
                    if (o%name /= 'foreach') then
                        reason = 'edit removes or changes '//o%name
                        return
                    end if
                    call loop_items(o, old_items, literal)
                    if (.not. literal) then
                        reason = 'edit changes a foreach without literal items'
                        return
                    end if
                    do i = from, n_new
                        associate (c => new(gap_new(i)))
                            if (c%name /= 'foreach') cycle
                            if (c%args(1)%s /= o%args(1)%s) cycle
                            call loop_items(c, new_items, literal)
                            if (.not. literal) cycle
                            call extension(old_items, new_items, extra, literal)
                            if (.not. literal) cycle
                        end associate
                        status(gap_new(i)) = MODIFIED
                        offsets(gap_new(i)) = size(added)
                        added = [added, extra]
                        from = i + 1
                        exit
                    end do
                    if (i > n_new) then
                        reason = 'edit removes foreach items or changes a loop'
                        return
                    end if
                end associate
            end do
            n_old = 0
            n_new = 0
        end subroutine close_gap

    end subroutine align

    subroutine loop_items(command, items, literal)
        !! Items of foreach(var item...) or foreach(var IN ITEMS item...).
        type(command_t), intent(in) :: command
        type(string_t), allocatable, intent(out) :: items(:)
        logical, intent(out) :: literal
        integer :: first, k

        literal = .false.
        allocate(items(0))
        if (size(command%args) < 1) return
        first = 2
        if (size(command%args) >= 3) then
            if (command%args(2)%s == 'IN') then
                if (command%args(3)%s /= 'ITEMS') return
                first = 4
            end if
        end if
        if (size(command%args) >= 2) then
            if (command%args(2)%s == 'RANGE') return
        end if
        do k = first, size(command%args)
            if (scan(command%args(k)%s, '$;') > 0) return
            items = [items, command%args(k)]
        end do
        literal = .true.
    end subroutine loop_items

    subroutine extension(old_items, new_items, extra, ok)
        !! old_items is a subsequence of new_items; extra holds the rest.
        type(string_t), intent(in) :: old_items(:), new_items(:)
        type(string_t), allocatable, intent(out) :: extra(:)
        logical, intent(out) :: ok
        integer :: i, k

        allocate(extra(0))
        i = 1
        do k = 1, size(new_items)
            if (i <= size(old_items)) then
                if (old_items(i)%s == new_items(k)%s) then
                    i = i + 1
                    cycle
                end if
            end if
            extra = [extra, new_items(k)]
        end do
        ok = i > size(old_items) .and. size(extra) > 0
    end subroutine extension

    subroutine parse_commands(text, commands, ok)
        !! The command invocations of a CMake listfile, without comments.
        !! Quoted and bracket arguments carry a leading achar(1) in the key.
        character(len=*), intent(in) :: text
        type(command_t), allocatable, intent(out) :: commands(:)
        logical, intent(out) :: ok
        type(command_t) :: c
        type(command_t), allocatable :: grown(:)
        character(len=:), allocatable :: key
        integer :: i, j, n, depth, start, count

        allocate(commands(64))
        count = 0
        ok = .false.
        n = len(text)
        i = 1
        do
            call skip_blank(text, i, ok)
            if (.not. ok) return
            ok = .false.
            if (i > n) exit
            if (.not. ident(text(i:i), .true.)) return
            start = i
            do while (i <= n)
                if (.not. ident(text(i:i), .false.)) exit
                i = i + 1
            end do
            c%name = to_lower(text(start:i - 1))
            do while (i <= n)
                if (text(i:i) /= ' ' .and. text(i:i) /= achar(9)) exit
                i = i + 1
            end do
            if (i > n) return
            if (text(i:i) /= '(') return
            i = i + 1
            depth = 0
            if (allocated(c%args)) deallocate(c%args)
            allocate(c%args(0))
            key = c%name//'('
            do
                call skip_blank(text, i, ok)
                if (.not. ok) return
                ok = .false.
                if (i > n) return
                select case (text(i:i))
                case (')')
                    i = i + 1
                    if (depth == 0) exit
                    depth = depth - 1
                    call add(')', .false.)
                case ('(')
                    i = i + 1
                    depth = depth + 1
                    call add('(', .false.)
                case ('"')
                    j = i + 1
                    do while (j <= n)
                        if (text(j:j) == '\') then
                            j = j + 2
                            cycle
                        end if
                        if (text(j:j) == '"') exit
                        j = j + 1
                    end do
                    if (j > n) return
                    call add(text(i + 1:j - 1), .true.)
                    i = j + 1
                case default
                    j = bracket_end(text, i)
                    if (j < 0) return
                    if (j > 0) then
                        call add(text(i:j), .true.)
                        i = j + 1
                        cycle
                    end if
                    j = i
                    do while (j <= n)
                        select case (text(j:j))
                        case (' ', achar(9), achar(10), achar(13), '(', ')')
                            exit
                        case ('\')
                            j = j + 2
                            cycle
                        case ('"')
                            j = j + 1
                            do while (j <= n)
                                if (text(j:j) == '\') then
                                    j = j + 2
                                    cycle
                                end if
                                if (text(j:j) == '"') exit
                                j = j + 1
                            end do
                        end select
                        j = j + 1
                    end do
                    j = min(j, n + 1)
                    call add(text(i:j - 1), .false.)
                    i = j
                end select
            end do
            c%key = key//')'
            if (count == size(commands)) then
                allocate(grown(2 * count))
                grown(1:count) = commands
                call move_alloc(grown, commands)
            end if
            count = count + 1
            commands(count) = c
        end do
        commands = commands(1:count)
        ok = .true.

    contains

        subroutine add(value, quoted)
            character(len=*), intent(in) :: value
            logical, intent(in) :: quoted

            c%args = [c%args, string_t(value)]
            if (quoted) then
                key = key//achar(1)//value//achar(30)
            else
                key = key//value//achar(30)
            end if
        end subroutine add

    end subroutine parse_commands

    subroutine skip_blank(text, i, ok)
        !! Skip whitespace, line comments and bracket comments.
        character(len=*), intent(in) :: text
        integer, intent(inout) :: i
        logical, intent(out) :: ok
        integer :: j

        ok = .true.
        do while (i <= len(text))
            select case (text(i:i))
            case (' ', achar(9), achar(10), achar(13))
                i = i + 1
            case ('#')
                j = 0
                if (i < len(text)) j = bracket_end(text, i + 1)
                if (j < 0) then
                    ok = .false.
                    return
                end if
                if (j > 0) then
                    i = j + 1
                    cycle
                end if
                j = index(text(i:), achar(10))
                if (j == 0) then
                    i = len(text) + 1
                else
                    i = i + j
                end if
            case default
                return
            end select
        end do
    end subroutine skip_blank

    integer function bracket_end(text, i) result(last)
        !! Last character of a bracket argument [=*[ ... ]=*] at i; 0 when
        !! none starts there, -1 when it is not closed.
        character(len=*), intent(in) :: text
        integer, intent(in) :: i
        integer :: k, close_at

        last = 0
        if (text(i:i) /= '[') return
        k = i + 1
        do while (k <= len(text))
            if (text(k:k) /= '=') exit
            k = k + 1
        end do
        if (k > len(text)) return
        if (text(k:k) /= '[') return
        close_at = index(text(k + 1:), ']'//repeat('=', k - i - 1)//']')
        if (close_at == 0) then
            last = -1
            return
        end if
        last = k + close_at + k - i
    end function bracket_end

    logical function ident(c, first)
        character, intent(in) :: c
        logical, intent(in) :: first

        ident = (c >= 'a' .and. c <= 'z') .or. (c >= 'A' .and. c <= 'Z') .or. c == '_'
        if (.not. first) ident = ident .or. (c >= '0' .and. c <= '9')
    end function ident

    integer function key_hash(text) result(h)
        character(len=*), intent(in) :: text
        integer(int64) :: x
        integer :: k

        x = 5381_int64
        do k = 1, len(text)
            x = iand(ieor(x * 33_int64, int(iachar(text(k:k)), int64)), 2147483647_int64)
        end do
        h = int(x)
    end function key_hash

    integer function keyword_at(args, word) result(at)
        type(string_t), intent(in) :: args(:)
        character(len=*), intent(in) :: word

        do at = 1, size(args)
            if (args(at)%s == word) return
        end do
        at = size(args) + 1
    end function keyword_at

    logical function any_named(names, name) result(found)
        type(string_t), intent(in) :: names(:)
        character(len=*), intent(in) :: name
        integer :: k

        found = .false.
        do k = 1, size(names)
            if (to_lower(names(k)%s) == name) found = .true.
        end do
    end function any_named

    function substitute(template, variable, value) result(out)
        character(len=*), intent(in) :: template, variable, value
        character(len=:), allocatable :: out
        character(len=:), allocatable :: ref
        integer :: at, from

        ref = '${'//variable//'}'
        out = ''
        from = 1
        do
            at = index(template(from:), ref)
            if (at == 0) exit
            out = out//template(from:from + at - 2)//value
            from = from + at - 1 + len(ref)
        end do
        out = out//template(from:)
    end function substitute

    function wildcard(template) result(out)
        !! Every variable reference and generator expression becomes '*'.
        character(len=*), intent(in) :: template
        character(len=:), allocatable :: out
        integer :: i, j, depth
        character :: open_c, close_c

        out = ''
        i = 1
        do while (i <= len(template))
            if (template(i:i) == '$' .and. i < len(template)) then
                j = i + 1
                if (template(j:j) == '<') then
                    open_c = '<'
                    close_c = '>'
                else
                    do while (j <= len(template))
                        if (.not. ident(template(j:j), .true.)) exit
                        j = j + 1
                    end do
                    open_c = '{'
                    close_c = '}'
                end if
                if (j <= len(template)) then
                    if (template(j:j) == open_c) then
                        depth = 0
                        do while (j <= len(template))
                            if (template(j:j) == open_c) depth = depth + 1
                            if (template(j:j) == close_c) depth = depth - 1
                            if (depth == 0) exit
                            j = j + 1
                        end do
                        if (len(out) == 0) then
                            out = '*'
                        else if (out(len(out):) /= '*') then
                            out = out//'*'
                        end if
                        i = j + 1
                        cycle
                    end if
                end if
            end if
            out = out//template(i:i)
            i = i + 1
        end do
    end function wildcard

    recursive logical function cmake_template_matches(template, name) result(matched)
        !! Whole-name match of a template in which '*' matches any text.
        character(len=*), intent(in) :: template, name
        integer :: star, k

        star = index(template, '*')
        if (star == 0) then
            matched = template == name
            return
        end if
        matched = .false.
        if (len(name) < star - 1) return
        if (template(1:star - 1) /= name(1:star - 1)) return
        do k = star, len(name) + 1
            if (cmake_template_matches(template(star + 1:), name(k:))) then
                matched = .true.
                return
            end if
        end do
    end function cmake_template_matches

end module fo_cmake_delta
