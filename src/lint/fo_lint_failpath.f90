module fo_lint_failpath
    !! What counts as a failure path, and which of a project's module procedures
    !! contain one.
    !!
    !! A statement can terminate the process with a nonzero status when it is
    !! `error stop` (with or without a code), `stop` with a nonzero or
    !! non-constant code, `call abort()`, or `call exit(...)` with a nonzero or
    !! non-constant status. A bare `stop`, `stop 0`, and a character stop code
    !! (`stop 'failed'`, which gfortran and ifort exit 0 on) are deliberately
    !! NOT failure paths.
    !!
    !! Test suites usually do not write those statements inline: they call a
    !! shared helper (`call test_suite_exit(suite)`) that holds the `stop 1`.
    !! To see that, this module scans the project's src/, app/ and test/ trees
    !! once and collects the names of module procedures whose own bodies can
    !! terminate nonzero, then repeats the scan so a procedure that only calls
    !! such a helper joins the set, until the set stops growing. A call to a
    !! collected name is then itself a failure path.
    !!
    !! Known limits, all of which report a test that can in fact fail (a false
    !! positive) rather than blessing one that cannot:
    !!   - a helper reached only through a procedure pointer or a generic
    !!     interface is not seen;
    !!   - a submodule separate body is not matched to its interface;
    !!   - only procedures at module level are collected, so an exit inside a
    !!     `contains`ed procedure does not mark its parent.
    !! The one unsafe direction is a name collision: two modules defining the
    !! same procedure name, one of which can fail, credit both.
    use fo_lint_lex, only: lex_read_logical_line, word_pos, next_token, &
        prev_token, skip_blanks
    use fo_fs, only: fs_collect_files
    implicit none
    private
    public :: failpath_line_exits_nonzero, failpath_load_helpers
    public :: failpath_line_calls_helper, failpath_helper_count

    integer, parameter :: MAXLEN = 4096
    integer, parameter :: MAX_HELPERS = 8192
    integer, parameter :: MAX_FILES = 20000
    integer, parameter :: MAX_PASSES = 4

    ! Helper set for the project loaded last, sorted by name for binary search.
    ! Written only inside failpath_load_helpers, which callers enter before any
    ! query, so the parallel lint pass sees a fully built set.
    character(len=64), save :: helper_name(MAX_HELPERS)
    logical, save :: helper_as_sub(MAX_HELPERS)
    logical, save :: helper_as_func(MAX_HELPERS)
    integer, save :: n_helpers = 0
    integer, save :: n_func_helpers = 0
    ! Which first letters any helper, and any helper callable as a function,
    ! start with. Every identifier on every line of the project is a candidate
    ! function call, and rejecting almost all of them on one character keeps
    ! the sweep off the binary search.
    logical, save :: func_first(0:255) = .false.
    character(len=1024), save :: loaded_root = ''
    logical, save :: root_loaded = .false.

contains

    subroutine failpath_load_helpers(root)
        !! Build (once per project root) the set of module procedures that can
        !! terminate nonzero. An empty root clears the set, which is what a
        !! source outside any fpm project gets.
        character(len=*), intent(in) :: root

        character(len=512), allocatable :: files(:)
        integer :: n_files, pass, added

        !$omp critical (fo_failpath_cache)
        if (.not. (root_loaded .and. trim(loaded_root) == trim(root))) then
            n_helpers = 0
            n_func_helpers = 0
            func_first = .false.
            if (len_trim(root) > 0) then
                allocate (files(MAX_FILES))
                call collect_project_sources(root, files, n_files)
                do pass = 1, MAX_PASSES
                    added = 0
                    call collect_pass(files, n_files, added)
                    if (added == 0) exit
                end do
                deallocate (files)
            end if
            loaded_root = root
            root_loaded = .true.
        end if
        !$omp end critical (fo_failpath_cache)
    end subroutine failpath_load_helpers

    integer function failpath_helper_count()
        !! Size of the loaded helper set. Exposed so a test can assert that
        !! collection found something rather than inferring it.
        failpath_helper_count = n_helpers
    end function failpath_helper_count

    subroutine collect_project_sources(root, files, n_files)
        character(len=*), intent(in) :: root
        character(len=512), intent(out) :: files(:)
        integer, intent(out) :: n_files

        character(len=512), allocatable :: hits(:)
        character(len=4), parameter :: suffixes(2) = ['.f90', '.F90']
        character(len=5), parameter :: trees(3) = ['/src ', '/app ', '/test']
        integer :: t, s, i, n_hits

        n_files = 0
        allocate (hits(MAX_FILES))
        do t = 1, size(trees)
            do s = 1, size(suffixes)
                call fs_collect_files(trim(root)//trim(trees(t)), '', &
                    trim(suffixes(s)), '', hits, n_hits)
                do i = 1, n_hits
                    if (n_files >= size(files)) exit
                    n_files = n_files + 1
                    files(n_files) = hits(i)
                end do
            end do
        end do
        deallocate (hits)
    end subroutine collect_project_sources

    subroutine collect_pass(files, n_files, added)
        !! One sweep over the project's sources. Repeating the sweep is what
        !! makes the set transitive: a helper that only calls another helper is
        !! recorded on the sweep after the one that recorded its callee.
        character(len=512), intent(in) :: files(:)
        integer, intent(in) :: n_files
        integer, intent(inout) :: added

        integer :: i

        do i = 1, n_files
            if (len_trim(files(i)) == 0) cycle
            call scan_source_for_helpers(trim(files(i)), added)
        end do
    end subroutine collect_pass

    subroutine scan_source_for_helpers(filepath, added)
        !! Record every module-level procedure in this file whose body holds a
        !! failure path, either written out or reached through an already
        !! recorded helper.
        character(len=*), intent(in) :: filepath
        integer, intent(inout) :: added

        character(len=MAXLEN) :: code
        character(len=64) :: name, top_name
        integer :: u, iostat, phys_no, start_line, depth, L
        logical :: in_module, is_func, top_is_func, opens, closes

        open (newunit=u, file=filepath, status='old', iostat=iostat)
        if (iostat /= 0) return
        phys_no = 0
        depth = 0
        in_module = .false.
        top_name = ''
        top_is_func = .false.
        do
            call lex_read_logical_line(u, code, start_line, phys_no, iostat)
            if (iostat /= 0) exit
            ! Hand on the trimmed line, not the 4 KB buffer: every token step
            ! below re-measures its argument, and measuring the padding once
            ! per token dominated the sweep.
            L = max(len_trim(code), 1)
            call classify_unit_statement(code(1:L), name, is_func, opens, closes)
            if (closes) then
                if (depth > 0) then
                    depth = depth - 1
                else
                    in_module = .false.
                end if
                cycle
            end if
            if (opens) then
                if (len_trim(name) == 0) then
                    in_module = .true.
                    depth = 0
                else
                    depth = depth + 1
                    if (depth == 1) then
                        top_name = name
                        top_is_func = is_func
                    end if
                end if
                cycle
            end if
            if (depth /= 1) cycle
            if (.not. in_module) cycle
            if (.not. line_is_failure_path(code(1:L))) cycle
            call add_helper(top_name, top_is_func, added)
        end do
        close (u)
    end subroutine scan_source_for_helpers

    logical function line_is_failure_path(code)
        !! A statement that exits nonzero itself, or that calls a procedure
        !! already known to.
        character(len=*), intent(in) :: code

        line_is_failure_path = failpath_line_exits_nonzero(code)
        if (.not. line_is_failure_path) &
            line_is_failure_path = failpath_line_calls_helper(code)
    end function line_is_failure_path

    subroutine classify_unit_statement(code, name, is_func, opens, closes)
        !! Recognise the statements that open or close a program unit. opens
        !! with an empty name is a module or submodule header; opens with a
        !! name is a procedure definition (or an interface body, which nests
        !! and closes the same way). closes is any `end` of a unit, as opposed
        !! to `end if` and its kin.
        character(len=*), intent(in) :: code
        character(len=*), intent(out) :: name
        logical, intent(out) :: is_func, opens, closes

        character(len=64) :: t1, t2
        integer :: after

        name = ''
        is_func = .false.
        opens = .false.
        closes = .false.
        call next_token(code, 1, t1, after)
        call next_token(code, after, t2, after)
        if (trim(t1) == 'end') then
            closes = is_unit_end(t2)
            return
        end if
        if (trim(t1) == 'submodule') then
            opens = .true.
            return
        end if
        if (trim(t1) == 'module') then
            if (trim(t2) == 'procedure') return
            if (trim(t2) /= 'subroutine' .and. trim(t2) /= 'function') then
                opens = .true.
                return
            end if
        end if
        if (trim(t1) == 'program') return
        call procedure_definition_name(code, name, is_func)
        opens = len_trim(name) > 0
    end subroutine classify_unit_statement

    logical function is_unit_end(t2)
        !! True for the `end` forms that close a program unit. A bare `end`
        !! (empty t2) closes whatever unit is open.
        character(len=*), intent(in) :: t2

        is_unit_end = len_trim(t2) == 0 .or. trim(t2) == 'subroutine' .or. &
            trim(t2) == 'function' .or. trim(t2) == 'module' .or. &
            trim(t2) == 'submodule' .or. trim(t2) == 'program'
    end function is_unit_end

    subroutine procedure_definition_name(code, name, is_func)
        !! Name introduced by a `subroutine NAME` or `function NAME` statement,
        !! whatever prefixes (recursive, pure, a result type) precede it.
        !! Callers must have ruled out `end subroutine` / `end function` first.
        character(len=*), intent(in) :: code
        character(len=*), intent(out) :: name
        logical, intent(out) :: is_func

        integer :: hit, after

        name = ''
        is_func = .false.
        call word_pos(code, 'subroutine', 1, hit)
        if (hit > 0) then
            call next_token(code, hit + 10, name, after)
            if (len_trim(name) > 0) return
        end if
        call word_pos(code, 'function', 1, hit)
        if (hit == 0) return
        call next_token(code, hit + 8, name, after)
        is_func = len_trim(name) > 0
    end subroutine procedure_definition_name

    subroutine add_helper(name, is_func, added)
        !! Insert into the sorted set, or widen an existing entry's calling
        !! form. Adding nothing new is how the sweep loop knows it has reached
        !! a fixed point.
        character(len=*), intent(in) :: name
        logical, intent(in) :: is_func
        integer, intent(inout) :: added

        integer :: slot, pos, i

        if (len_trim(name) == 0) return
        call find_helper(name, slot, pos)
        if (slot > 0) then
            if (is_func) then
                if (helper_as_func(slot)) return
                helper_as_func(slot) = .true.
                call note_func_first(name)
            else
                if (helper_as_sub(slot)) return
                helper_as_sub(slot) = .true.
            end if
            added = added + 1
            return
        end if
        if (n_helpers >= MAX_HELPERS) return
        do i = n_helpers, pos, -1
            helper_name(i + 1) = helper_name(i)
            helper_as_sub(i + 1) = helper_as_sub(i)
            helper_as_func(i + 1) = helper_as_func(i)
        end do
        helper_name(pos) = name
        helper_as_sub(pos) = .not. is_func
        helper_as_func(pos) = is_func
        n_helpers = n_helpers + 1
        if (is_func) call note_func_first(name)
        added = added + 1
    end subroutine add_helper

    subroutine note_func_first(name)
        character(len=*), intent(in) :: name

        n_func_helpers = n_func_helpers + 1
        func_first(iachar(name(1:1))) = .true.
    end subroutine note_func_first

    subroutine find_helper(name, slot, pos)
        !! Binary search. slot is the entry's index, 0 when absent; pos is where
        !! an absent name would be inserted to keep the set sorted.
        character(len=*), intent(in) :: name
        integer, intent(out) :: slot, pos

        integer :: lo, hi, mid

        slot = 0
        lo = 1
        hi = n_helpers
        do while (lo <= hi)
            mid = (lo + hi)/2
            if (helper_name(mid) == name) then
                slot = mid
                pos = mid
                return
            end if
            if (llt(helper_name(mid), name)) then
                lo = mid + 1
            else
                hi = mid - 1
            end if
        end do
        pos = lo
    end subroutine find_helper

    logical function is_helper(name, want_func)
        character(len=*), intent(in) :: name
        logical, intent(in) :: want_func

        integer :: slot, pos

        is_helper = .false.
        call find_helper(name, slot, pos)
        if (slot == 0) return
        if (want_func) then
            is_helper = helper_as_func(slot)
        else
            is_helper = helper_as_sub(slot)
        end if
    end function is_helper

    logical function failpath_line_calls_helper(code)
        !! True if the masked logical line calls a procedure from the loaded
        !! set. A subroutine has to be reached through `call`; a function has to
        !! be followed by its argument list, so a variable of the same name is
        !! not mistaken for it.
        character(len=*), intent(in) :: code

        character(len=64) :: name
        integer :: p, hit, after

        failpath_line_calls_helper = .false.
        if (n_helpers == 0) return
        p = 1
        do
            call word_pos(code, 'call', p, hit)
            if (hit == 0) exit
            call next_token(code, hit + 4, name, after)
            if (is_helper(name, .false.)) then
                failpath_line_calls_helper = .true.
                return
            end if
            p = hit + 4
        end do
        if (n_func_helpers == 0) return
        failpath_line_calls_helper = line_calls_helper_func(code)
    end function failpath_line_calls_helper

    logical function line_calls_helper_func(code)
        character(len=*), intent(in) :: code

        character(len=64) :: name
        integer :: p, after, L, q

        line_calls_helper_func = .false.
        L = len_trim(code)
        p = 1
        do
            if (p > L) return
            call next_token(code, p, name, after)
            if (len_trim(name) == 0) then
                p = skip_blanks(code, p) + 1
                cycle
            end if
            if (.not. func_first(iachar(name(1:1)))) then
                p = after
                cycle
            end if
            q = skip_blanks(code, after)
            if (q <= L) then
                if (code(q:q) == '(') then
                    if (is_helper(name, .true.)) then
                        line_calls_helper_func = .true.
                        return
                    end if
                end if
            end if
            p = after
        end do
    end function line_calls_helper_func

    logical function failpath_line_exits_nonzero(code)
        !! True if the masked logical line holds a statement that can terminate
        !! the program with a nonzero status.
        character(len=*), intent(in) :: code

        failpath_line_exits_nonzero = has_error_stop(code)
        if (.not. failpath_line_exits_nonzero) &
            failpath_line_exits_nonzero = has_nonzero_stop(code)
        if (.not. failpath_line_exits_nonzero) &
            failpath_line_exits_nonzero = has_abort_call(code)
        if (.not. failpath_line_exits_nonzero) &
            failpath_line_exits_nonzero = has_nonzero_exit(code)
    end function failpath_line_exits_nonzero

    logical function has_error_stop(code)
        !! `error stop` terminates with a nonzero status whether or not a stop
        !! code is given, so the code itself does not matter here.
        character(len=*), intent(in) :: code

        character(len=128) :: tok
        integer :: p, hit, after

        has_error_stop = .false.
        p = 1
        do
            call word_pos(code, 'error', p, hit)
            if (hit == 0) return
            call next_token(code, hit + 5, tok, after)
            if (trim(tok) == 'stop') then
                has_error_stop = .true.
                return
            end if
            p = hit + 5
        end do
    end function has_error_stop

    logical function has_nonzero_stop(code)
        character(len=*), intent(in) :: code

        integer :: p, hit

        has_nonzero_stop = .false.
        p = 1
        do
            call word_pos(code, 'stop', p, hit)
            if (hit == 0) return
            if (code_arg_nonzero(code, hit + 4)) then
                has_nonzero_stop = .true.
                return
            end if
            p = hit + 4
        end do
    end function has_nonzero_stop

    logical function has_abort_call(code)
        !! `call abort()` raises SIGABRT: the process dies with a nonzero status.
        character(len=*), intent(in) :: code

        has_abort_call = called_word(code, 'abort', 0) > 0
    end function has_abort_call

    logical function has_nonzero_exit(code)
        !! `call exit(status)` from the GNU extension family.
        character(len=*), intent(in) :: code

        integer :: p, open_paren

        has_nonzero_exit = .false.
        p = 1
        do
            open_paren = called_word(code, 'exit', p)
            if (open_paren == 0) return
            if (code_arg_nonzero(code, open_paren + 1)) then
                has_nonzero_exit = .true.
                return
            end if
            p = open_paren
        end do
    end function has_nonzero_exit

    integer function called_word(code, word, from)
        !! Position of the '(' that follows `call WORD` at or after from, or the
        !! position just past WORD when it takes no argument list. 0 if `call
        !! WORD` does not occur. Requiring the `call` keyword keeps the loop
        !! statement `exit` and any variable named exit out of the match.
        character(len=*), intent(in) :: code
        character(len=*), intent(in) :: word
        integer, intent(in) :: from

        character(len=128) :: prev
        integer :: p, q, hit

        called_word = 0
        p = max(from, 1)
        do
            call word_pos(code, word, p, hit)
            if (hit == 0) return
            call prev_token(code, hit - 1, prev)
            if (trim(prev) == 'call') then
                q = skip_blanks(code, hit + len(word))
                called_word = q
                if (q <= len_trim(code)) then
                    if (code(q:q) /= '(') called_word = hit + len(word)
                end if
                return
            end if
            p = hit + len(word)
        end do
    end function called_word

    logical function code_arg_nonzero(code, from)
        !! Read the stop code or exit status starting at from and decide whether
        !! it can be nonzero. Nothing there (a bare `stop`, or a character stop
        !! code, which masking blanked) means the program exits 0. A literal 0
        !! means the same. Anything else - a nonzero literal, a variable, an
        !! expression - is assumed to be able to fail.
        character(len=*), intent(in) :: code
        integer, intent(in) :: from

        character(len=128) :: tok
        integer :: p, after, value, iostat

        code_arg_nonzero = .false.
        p = skip_blanks(code, from)
        if (p > len_trim(code)) return
        call next_token(code, p, tok, after)
        if (len_trim(tok) == 0) then
            code_arg_nonzero = .true.
            return
        end if
        if (.not. all_digits(tok)) then
            code_arg_nonzero = .true.
            return
        end if
        read (tok, *, iostat=iostat) value
        if (iostat /= 0) then
            code_arg_nonzero = .true.
            return
        end if
        code_arg_nonzero = value /= 0
    end function code_arg_nonzero

    logical function all_digits(tok)
        character(len=*), intent(in) :: tok
        integer :: i, ic

        all_digits = len_trim(tok) > 0
        do i = 1, len_trim(tok)
            ic = iachar(tok(i:i))
            if (ic < iachar('0') .or. ic > iachar('9')) then
                all_digits = .false.
                return
            end if
        end do
    end function all_digits

end module fo_lint_failpath
