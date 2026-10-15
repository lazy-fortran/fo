module fo_format
    use fx_string, only: to_lower
    implicit none
    private
    public :: format_lines, format_file, format_file_with_indent, MAX_LINE_LEN

    ! Width of the fixed-length buffers taken by format_lines. File formatting
    ! uses per-line allocatable strings and has no line length or count limit.
    integer, parameter :: MAX_LINE_LEN = 512
    integer, parameter :: INDENT_WIDTH = 4
    integer, parameter :: MAX_INDENT_SPACES = 1024
    integer, parameter :: READ_CHUNK = 4096

    type :: line_t
        character(len=:), allocatable :: text
    end type line_t

contains

    ! Format lines in memory. Input lines have len >= MAX_LINE_LEN.
    ! n_output is set to the number of formatted output lines.
    subroutine format_lines(lines, n_input, output, n_output)
        character(len=MAX_LINE_LEN), intent(in) :: lines(n_input)
        integer, intent(in) :: n_input
        character(len=MAX_LINE_LEN), intent(out) :: output(n_input)
        integer, intent(out) :: n_output

        type(line_t), allocatable :: in_lines(:), out_lines(:)
        integer :: i

        allocate (in_lines(n_input), out_lines(n_input))
        do i = 1, n_input
            in_lines(i)%text = trim(lines(i))
        end do
        call format_line_list(in_lines, n_input, out_lines, n_output, INDENT_WIDTH)
        do i = 1, n_input
            output(i) = ''
            if (i <= n_output) output(i) = out_lines(i)%text
        end do
    end subroutine format_lines

    subroutine format_line_list(lines, n_input, output, n_output, indent_width)
        type(line_t), intent(in) :: lines(:)
        integer, intent(in) :: n_input
        type(line_t), intent(inout) :: output(:)
        integer, intent(out) :: n_output
        integer, intent(in) :: indent_width

        character(len=:), allocatable :: prev_content, stmt_head
        character(len=1) :: quote_state, quote_before
        integer :: i, indent_level, opens, closes, is_both, width, wl
        logical :: in_continuation

        n_output = n_input
        indent_level = 0
        in_continuation = .false.
        prev_content = ''
        stmt_head = ''
        quote_state = ' '
        width = INDENT_WIDTH
        if (indent_width > 0) width = indent_width

        do i = 1, n_input
            wl = len_trim(lines(i)%text) + 1
            block
                character(len=:), allocatable :: content, comment, masked, out_line

                allocate (character(len=wl) :: content, comment, masked)
                allocate (character(len=wl + MAX_INDENT_SPACES + 1) :: out_line)

                ! Blank, comment and directive lines preserve a pending
                ! continuation.
                quote_before = quote_state
                call split_line(lines(i)%text, content, comment, quote_state)

                ! Preprocessor directives pass through unchanged. content has
                ! at least one character and is '#' only for a non-blank line.
                if (content(1:1) == '#') then
                    output(i)%text = lines(i)%text
                    cycle
                end if

                ! Truly empty line: preserve as blank.
                if (len_trim(content) == 0 .and. len_trim(comment) == 0) then
                    output(i)%text = ''
                    cycle
                end if

                ! Comment-only line: apply current indent to the comment.
                if (len_trim(content) == 0 .and. len_trim(comment) > 0) then
                    call apply_indent('', comment, indent_level, width, out_line)
                    output(i)%text = trim(out_line)
                    cycle
                end if

                call mask_strings(content, masked, quote_before)

                ! Classify this line using the masked content.
                call classify_line(masked, opens, closes, is_both)

                ! Detect continuation: previous (non-empty) content ended with &
                in_continuation = continuation_pending(prev_content)

                ! Decrease indent before emitting close keywords.
                if (closes > 0 .and. .not. in_continuation) then
                    indent_level = max(0, indent_level - closes)
                end if

                ! Emit with current indent.
                if (in_continuation) then
                    call apply_indent(content, comment, indent_level + 1, width, &
                        out_line)
                else
                    call apply_indent(content, comment, indent_level, width, out_line)
                end if
                output(i)%text = trim(out_line)

                ! Increase indent after emitting open keywords.
                if (opens > 0 .and. .not. in_continuation) then
                    indent_level = indent_level + opens
                end if

                ! A block-if whose `then` is pushed onto a continuation line: the
                ! head line (ending in '&') had no `then`, so classify_line scored
                ! opens=0. Open the block when the final continuation line ends in
                ! `then` and the statement head is a plain `if (` (an `else if`
                ! head already opened, so it is excluded).
                if (in_continuation .and. .not. continuation_pending(content)) then
                    if (has_keyword_then(to_lower(masked)) .and. &
                        head_opens_block_if(stmt_head)) then
                        indent_level = indent_level + 1
                    end if
                end if

                if (.not. in_continuation) stmt_head = trim(masked)
                prev_content = trim(content)
            end block
        end do
    end subroutine format_line_list

    ! Format a file in place. Returns exitcode 0 on success, 1 on I/O error.
    subroutine format_file(filepath, exitcode)
        character(len=*), intent(in) :: filepath
        integer, intent(out) :: exitcode

        call format_file_with_indent(filepath, exitcode, INDENT_WIDTH)
    end subroutine format_file

    ! The file is left untouched when it cannot be read completely.
    subroutine format_file_with_indent(filepath, exitcode, indent_width)
        character(len=*), intent(in) :: filepath
        integer, intent(out) :: exitcode
        integer, intent(in) :: indent_width

        type(line_t), allocatable :: lines(:), output(:)
        integer :: n_lines, n_output, u, ios, i

        exitcode = 0
        open (newunit=u, file=trim(filepath), status='old', &
            action='read', iostat=ios)
        if (ios /= 0) then
            exitcode = 1
            return
        end if
        allocate (lines(1024))
        n_lines = 0
        do
            if (n_lines == size(lines)) call grow_lines(lines)
            call read_line(u, lines(n_lines + 1)%text, ios)
            if (ios /= 0) exit
            n_lines = n_lines + 1
        end do
        close (u)
        if (.not. is_iostat_end(ios)) then
            exitcode = 1
            return
        end if

        allocate (output(max(n_lines, 1)))
        call format_line_list(lines, n_lines, output, n_output, indent_width)

        open (newunit=u, file=trim(filepath), status='replace', &
            action='write', iostat=ios)
        if (ios /= 0) then
            exitcode = 1
            return
        end if
        do i = 1, n_output
            write (u, '(a)') trim(output(i)%text)
        end do
        close (u)
    end subroutine format_file_with_indent

    subroutine grow_lines(lines)
        type(line_t), allocatable, intent(inout) :: lines(:)

        type(line_t), allocatable :: bigger(:)
        integer :: i

        allocate (bigger(2*size(lines)))
        do i = 1, size(lines)
            call move_alloc(lines(i)%text, bigger(i)%text)
        end do
        call move_alloc(bigger, lines)
    end subroutine grow_lines

    ! Read one record of any length. ios is 0, iostat_end at end of file, or
    ! another nonzero value on a read error.
    subroutine read_line(u, line, ios)
        integer, intent(in) :: u
        character(len=:), allocatable, intent(out) :: line
        integer, intent(out) :: ios

        character(len=READ_CHUNK) :: chunk
        integer :: got

        line = ''
        do
            got = 0
            read (u, '(a)', advance='no', size=got, iostat=ios) chunk
            if (got > 0) line = line//chunk(1:got)
            if (is_iostat_eor(ios)) then
                ios = 0
                return
            end if
            if (ios /= 0) then
                ! A final record without newline arrives with its end condition.
                if (is_iostat_end(ios) .and. len(line) > 0) ios = 0
                return
            end if
        end do
    end subroutine read_line

    ! Split a source line into content (stripped of leading whitespace) and
    ! the trailing comment (starting with '!', including the '!').
    ! String literals protect '!' characters inside them from being misread.
    subroutine split_line(line, content, comment, quote_state)
        character(len=*), intent(in) :: line
        character(len=*), intent(out) :: content, comment
        character(len=1), intent(inout) :: quote_state

        integer :: i, n
        logical :: in_single, in_double
        character(len=1) :: ch

        comment = ''
        content = adjustl(line)
        ! Whole-line comments can separate continued character-literal segments.
        if (content(1:1) == '!') then
            comment = content
            content = ''
            return
        end if
        if (len_trim(content) == 0 .or. content(1:1) == '#') return
        n = len_trim(line)
        in_single = quote_state == "'"
        in_double = quote_state == '"'

        do i = 1, n
            ch = line(i:i)
            if (in_single) then
                if (ch == "'") in_single = .false.
                cycle
            end if
            if (in_double) then
                if (ch == '"') in_double = .false.
                cycle
            end if
            if (ch == "'") then
                in_single = .true.
                cycle
            end if
            if (ch == '"') then
                in_double = .true.
                cycle
            end if
            if (ch == '!') then
                comment = line(i:n)
                content = adjustl(line(1:i - 1))
                call rstrip(content)
                quote_state = ' '
                return
            end if
        end do

        content = adjustl(line)
        call rstrip(content)
        quote_state = ' '
        if (continuation_pending(content)) then
            if (in_single) quote_state = "'"
            if (in_double) quote_state = '"'
        end if
    end subroutine split_line

    ! Replace string literal contents with spaces (so keyword detection
    ! ignores text inside quotes).
    subroutine mask_strings(line, masked, initial_quote)
        character(len=*), intent(in) :: line
        character(len=*), intent(out) :: masked
        character(len=1), intent(in) :: initial_quote

        integer :: i, n
        logical :: in_single, in_double
        character(len=1) :: ch

        masked = line
        n = len_trim(line)
        in_single = initial_quote == "'"
        in_double = initial_quote == '"'

        do i = 1, n
            ch = line(i:i)
            if (in_single) then
                if (ch == "'") then
                    in_single = .false.
                else
                    masked(i:i) = ' '
                end if
                cycle
            end if
            if (in_double) then
                if (ch == '"') then
                    in_double = .false.
                else
                    masked(i:i) = ' '
                end if
                cycle
            end if
            if (ch == "'") then
                in_single = .true.
                cycle
            end if
            if (ch == '"') then
                in_double = .true.
                cycle
            end if
        end do
    end subroutine mask_strings

    ! Classify the (lowercased, masked) content line:
    !   opens  — net number of indent levels opened after this line
    !   closes — net number of indent levels closed before this line
    !   is_both — number of levels both closed before AND opened after
    !             (used by dual keywords: else, elsewhere, case, contains)
    !
    ! Rule: the caller decreases indent by closes before emitting the line,
    ! then increases by opens after emitting. For "both" keywords the same
    ! values handle the dual nature.
    subroutine classify_line(masked, opens, closes, is_both)
        character(len=*), intent(in) :: masked
        integer, intent(out) :: opens, closes, is_both

        character(len=len_trim(masked)) :: low
        integer :: n

        opens = 0
        closes = 0
        is_both = 0

        low = to_lower(masked)
        call rstrip(low)
        call strip_construct_name(low)
        n = len_trim(low)
        if (n == 0) return

        ! ---- keywords that ONLY close (end X) ----
        if (matches(low, 'end do') .or. kw_eq(low, 'end do') .or. &
            kw_eq(low, 'enddo')) then
            closes = 1; return
        end if
        if (matches(low, 'end if') .or. kw_eq(low, 'end if') .or. &
            kw_eq(low, 'endif')) then
            closes = 1; return
        end if
        if (matches(low, 'end select') .or. kw_eq(low, 'endselect')) then
            closes = 1; return
        end if
        if (matches(low, 'end where') .or. kw_eq(low, 'endwhere')) then
            closes = 1; return
        end if
        if (matches(low, 'end forall') .or. kw_eq(low, 'endforall')) then
            closes = 1; return
        end if
        if (matches(low, 'end associate') .or. kw_eq(low, 'endassociate')) then
            closes = 1; return
        end if
        if (kw_eq(low, 'end block') .or. kw_eq(low, 'endblock') .or. &
            kw_eq(low, 'end block data') .or. kw_eq(low, 'endblockdata')) then
            closes = 1; return
        end if
        if (matches(low, 'end type') .or. kw_eq(low, 'endtype')) then
            closes = 1; return
        end if
        if (matches(low, 'end interface') .or. kw_eq(low, 'endinterface')) then
            closes = 1; return
        end if
        if (matches(low, 'end module') .or. kw_eq(low, 'endmodule')) then
            closes = 1; return
        end if
        if (matches(low, 'end submodule') .or. kw_eq(low, 'endsubmodule')) then
            closes = 1; return
        end if
        if (matches(low, 'end program') .or. kw_eq(low, 'endprogram')) then
            closes = 1; return
        end if
        if (matches(low, 'end subroutine') .or. kw_eq(low, 'endsubroutine')) then
            closes = 1; return
        end if
        if (matches(low, 'end function') .or. kw_eq(low, 'endfunction')) then
            closes = 1; return
        end if
        if (matches(low, 'end critical') .or. kw_eq(low, 'endcritical')) then
            closes = 1; return
        end if

        ! ---- keywords that BOTH close and open (dual) ----
        if (kw_eq(low, 'else') .or. &
            kw_starts(low, 'else if') .or. kw_starts(low, 'elseif')) then
            closes = 1; opens = 1; is_both = 1; return
        end if
        if (kw_eq(low, 'elsewhere') .or. kw_starts(low, 'elsewhere')) then
            closes = 1; opens = 1; is_both = 1; return
        end if
        if (kw_starts(low, 'case ') .or. kw_eq(low, 'case default') .or. &
            kw_starts(low, 'case(') .or. kw_starts(low, 'class is') .or. &
            kw_starts(low, 'class default')) then
            closes = 1; opens = 1; is_both = 1; return
        end if
        if (kw_eq(low, 'contains')) then
            closes = 1; opens = 1; is_both = 1; return
        end if

        ! ---- keywords that ONLY open ----
        ! do loop: bare 'do', 'do i=1,n', 'do while(...)' but not 'end do'
        ! Skip: 'do concurrent' is still a block opener.
        if (kw_eq(low, 'do') .or. kw_starts(low, 'do ') .or. &
            kw_starts(low, 'do,')) then
            ! do not count 'done', 'double' etc.
            opens = 1; return
        end if

        ! if (...) then  — only block form
        if (kw_starts(low, 'if ') .or. kw_starts(low, 'if(')) then
            if (has_keyword_then(low)) then
                opens = 1
            end if
            return
        end if

        if (kw_starts(low, 'select case') .or. &
            kw_starts(low, 'select type') .or. &
            kw_starts(low, 'select rank')) then
            opens = 1; return
        end if

        ! where block form only: 'where (cond)' with nothing after the paren
        if (kw_starts(low, 'where ') .or. kw_starts(low, 'where(')) then
            if (is_block_where(low)) opens = 1
            return
        end if

        if (kw_starts(low, 'forall ') .or. kw_starts(low, 'forall(')) then
            if (is_block_forall(low)) opens = 1
            return
        end if

        if (kw_starts(low, 'associate ') .or. kw_starts(low, 'associate(')) then
            opens = 1; return
        end if

        ! bare 'block' (not 'block data')
        if (kw_eq(low, 'block')) then
            opens = 1; return
        end if

        ! critical (OpenMP-style)
        if (kw_starts(low, 'critical ') .or. kw_eq(low, 'critical') .or. &
            kw_starts(low, 'critical(')) then
            opens = 1; return
        end if

        ! type :: name or type, attr :: name (not type(...)  which is a cast)
        if (kw_starts(low, 'type ') .or. kw_starts(low, 'type,')) then
            if (is_type_def(low)) opens = 1
            return
        end if

        if (kw_eq(low, 'interface') .or. &
            kw_starts(low, 'interface ') .or. &
            kw_starts(low, 'abstract interface')) then
            opens = 1; return
        end if

        if (kw_starts(low, 'module ') .and. .not. &
            kw_starts(low, 'module procedure')) then
            opens = 1; return
        end if

        if (kw_starts(low, 'submodule ')) then
            opens = 1; return
        end if

        if (kw_starts(low, 'program ')) then
            opens = 1; return
        end if

        ! subroutine: may start with a prefix (pure/elemental/recursive/module)
        if (has_subroutine_opener(low)) then
            opens = 1; return
        end if

        ! function: may start with type prefix (e.g. 'real function foo(...)')
        if (has_function_opener(low)) then
            opens = 1; return
        end if
    end subroutine classify_line

    ! Remove a leading construct name ("outer: if (...) then") so the block
    ! keyword is classified as if it were unnamed. A "::" is not a construct
    ! name separator, and neither is a colon inside parentheses.
    subroutine strip_construct_name(low)
        character(len=*), intent(inout) :: low
        integer :: i, n, c

        n = len_trim(low)
        i = 1
        do while (i <= n)
            c = iachar(low(i:i))
            if (.not. ((c >= 97 .and. c <= 122) .or. c == 95 .or. &
                (i > 1 .and. c >= 48 .and. c <= 57))) exit
            i = i + 1
        end do
        if (i == 1) return
        do while (i <= n)
            if (low(i:i) /= ' ') exit
            i = i + 1
        end do
        if (i >= n) return
        if (low(i:i) /= ':') return
        if (low(i + 1:i + 1) == ':') return
        low = adjustl(low(i + 1:))
    end subroutine strip_construct_name

    ! Apply indent to content and comment, write result to out_line.
    subroutine apply_indent(content, comment, level, indent_width, out_line)
        character(len=*), intent(in) :: content, comment
        integer, intent(in) :: level, indent_width
        character(len=*), intent(out) :: out_line

        integer :: nsp
        character(len=1024) :: spaces

        nsp = level*indent_width
        if (nsp < 0) nsp = 0
        if (nsp > len(spaces)) nsp = len(spaces)
        spaces = repeat(' ', nsp)

        if (len_trim(content) == 0 .and. len_trim(comment) > 0) then
            ! comment-only line: indent the comment
            out_line = spaces(1:nsp)//trim(comment)
        else if (len_trim(content) > 0 .and. len_trim(comment) > 0) then
            out_line = spaces(1:nsp)//trim(content)//' '//trim(comment)
        else
            out_line = spaces(1:nsp)//trim(content)
        end if
    end subroutine apply_indent

    ! Strip trailing whitespace in place.
    subroutine rstrip(s)
        character(len=*), intent(inout) :: s
        integer :: n
        n = len_trim(s)
        s = s(1:n)
    end subroutine rstrip

    ! Return .true. if the (stripped, masked) line ends with '&' after
    ! removing trailing comment (already done in split_line).
    logical function continuation_pending(content)
        character(len=*), intent(in) :: content
        integer :: n
        n = len_trim(content)
        continuation_pending = .false.
        if (n > 0) continuation_pending = (content(n:n) == '&')
    end function continuation_pending

    ! Case-insensitive tolower for ASCII.
    ! True if the lowercased line starts with kw followed by space, '(', or EOL.
    logical function kw_starts(line, kw)
        character(len=*), intent(in) :: line, kw
        integer :: n
        n = len(kw)
        ! len_trim(line) >= n implies len(line) >= n, so the substring is in range
        ! whenever the guard holds; keep them in separate tests (no short-circuit).
        kw_starts = .false.
        if (len_trim(line) >= n) kw_starts = line(1:n) == kw
    end function kw_starts

    ! True if the entire (trimmed) lowercased line equals kw or kw+optional name.
    logical function kw_eq(line, kw)
        character(len=*), intent(in) :: line, kw
        integer :: n
        n = len(kw)
        kw_eq = .false.
        if (len_trim(line) == n) then
            kw_eq = line(1:n) == kw
        else if (len_trim(line) > n) then
            kw_eq = line(1:n) == kw .and. line(n + 1:n + 1) == ' '
        end if
    end function kw_eq

    ! True if trimmed line exactly equals kw, or starts with 'kw ' or 'kw('.
    logical function matches(line, kw)
        character(len=*), intent(in) :: line, kw
        integer :: n, tlen
        n = len(kw)
        tlen = len_trim(line)
        matches = .false.
        if (tlen < n) return
        if (line(1:n) /= kw) return
        if (tlen == n) then
            matches = .true.
        else
            matches = (line(n + 1:n + 1) == ' ')
        end if
    end function matches

    ! True if the (masked) statement head opens a block-if, i.e. begins with
    ! 'if (' or 'if(' (a plain if). An 'else if' head already opens via
    ! classify_line, so it is deliberately excluded here.
    logical function head_opens_block_if(head)
        character(len=*), intent(in) :: head
        character(len=len(head)) :: low
        low = to_lower(head)
        call rstrip(low)
        head_opens_block_if = kw_starts(low, 'if ') .or. kw_starts(low, 'if(')
    end function head_opens_block_if

    ! True if 'if (...) then' block form (not single-line if).
    logical function has_keyword_then(low)
        character(len=*), intent(in) :: low
        integer :: n
        n = len_trim(low)
        has_keyword_then = .false.
        ! Look for 'then' at end of line (possibly after closing paren/space).
        if (n >= 4) then
            if (low(n - 3:n) == 'then') then
                has_keyword_then = .true.
            end if
        end if
    end function has_keyword_then

    ! True if 'where (cond)' is a block form (nothing meaningful after the
    ! closing paren of the condition).
    logical function is_block_where(low)
        character(len=*), intent(in) :: low
        integer :: depth, i, n
        character(len=1) :: ch
        n = len_trim(low)
        depth = 0
        is_block_where = .false.
        do i = 1, n
            ch = low(i:i)
            if (ch == '(') depth = depth + 1
            if (ch == ')') then
                depth = depth - 1
                if (depth == 0) then
                    ! check nothing meaningful follows the closing paren
                    is_block_where = (len_trim(low(i + 1:n)) == 0)
                    return
                end if
            end if
        end do
    end function is_block_where

    ! True if 'forall (spec)' is a block form (nothing after closing paren).
    logical function is_block_forall(low)
        character(len=*), intent(in) :: low
        integer :: depth, i, n
        character(len=1) :: ch
        n = len_trim(low)
        depth = 0
        is_block_forall = .false.
        do i = 1, n
            ch = low(i:i)
            if (ch == '(') depth = depth + 1
            if (ch == ')') then
                depth = depth - 1
                if (depth == 0) then
                    is_block_forall = (len_trim(low(i + 1:n)) == 0)
                    return
                end if
            end if
        end do
    end function is_block_forall

    ! True if 'type ...' is a type definition (has '::'), not a type cast.
    logical function is_type_def(low)
        character(len=*), intent(in) :: low
        is_type_def = (index(low, '::') > 0)
    end function is_type_def

    ! True if the line contains a function opener. Handles prefixes like
    ! 'pure', 'elemental', 'recursive', 'real', 'integer', etc.
    logical function has_function_opener(low)
        character(len=*), intent(in) :: low
        has_function_opener = procedure_keyword_opens(low, 'function')
    end function has_function_opener

    ! True when position pos begins a word, i.e. the character before it cannot
    ! be part of a Fortran identifier.
    pure logical function starts_word(low, pos)
        character(len=*), intent(in) :: low
        integer, intent(in) :: pos
        character :: prev
        starts_word = .true.
        if (pos <= 1) return
        prev = low(pos - 1:pos - 1)
        if (prev == '_') then
            starts_word = .false.
        else if (prev >= 'a' .and. prev <= 'z') then
            starts_word = .false.
        else if (prev >= '0' .and. prev <= '9') then
            starts_word = .false.
        end if
    end function starts_word

    ! A subroutine definition, allowing a leading prefix such as
    ! 'pure', 'elemental', 'recursive', or 'module subroutine'.
    logical function has_subroutine_opener(low)
        character(len=*), intent(in) :: low
        has_subroutine_opener = procedure_keyword_opens(low, 'subroutine')
    end function has_subroutine_opener

    logical function procedure_keyword_opens(low, keyword) result(opens)
        character(len=*), intent(in) :: low, keyword
        integer :: from, pos, after, n
        character :: next

        opens = .false.
        from = 1
        n = len_trim(low)
        do while (from <= n)
            pos = index(low(from:), keyword//' ')
            if (pos == 0) return
            pos = pos + from - 1
            from = pos + len(keyword)
            if (.not. starts_word(low, pos)) cycle
            if (pos >= 5) then
                if (low(pos - 4:pos - 1) == 'end ') cycle
            end if
            ! A definition needs a procedure name after the keyword. Fortran
            ! also permits variables named FUNCTION or SUBROUTINE, so a whole
            ! word followed by assignment, association or '(' is not a header.
            after = from
            do while (after <= n)
                if (low(after:after) /= ' ') exit
                after = after + 1
            end do
            if (after > n) return
            next = low(after:after)
            if (next < 'a' .or. next > 'z') cycle
            opens = .true.
            return
        end do
    end function procedure_keyword_opens

end module fo_format
