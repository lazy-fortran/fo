module fo_format
    implicit none
    private
    public :: format_lines, format_file, MAX_LINE_LEN

    integer, parameter :: MAX_LINE_LEN = 512
    integer, parameter :: INDENT_WIDTH = 4
    integer, parameter :: MAX_LINES = 50000

contains

    ! Format lines in memory. Input lines have len >= MAX_LINE_LEN.
    ! n_output is set to the number of formatted output lines.
    subroutine format_lines(lines, n_input, output, n_output)
        character(len=MAX_LINE_LEN), intent(in) :: lines(n_input)
        integer, intent(in) :: n_input
        character(len=MAX_LINE_LEN), intent(out) :: output(n_input)
        integer, intent(out) :: n_output

        character(len=MAX_LINE_LEN) :: content, comment, masked
        character(len=MAX_LINE_LEN) :: prev_content
        integer :: i, indent_level, opens, closes, is_both
        logical :: in_continuation

        n_output = n_input
        indent_level = 0
        in_continuation = .false.
        prev_content = ''

        do i = 1, n_input
            call split_line(lines(i), content, comment)

            ! Preprocessor directives: pass through unchanged.
            if (len_trim(content) > 0 .and. content(1:1) == '#') then
                output(i) = lines(i)
                in_continuation = .false.
                prev_content = content
                cycle
            end if

            ! Truly empty line: preserve as blank.
            if (len_trim(content) == 0 .and. len_trim(comment) == 0) then
                output(i) = ''
                in_continuation = .false.
                prev_content = content
                cycle
            end if

            ! Comment-only line: apply current indent to the comment.
            if (len_trim(content) == 0 .and. len_trim(comment) > 0) then
                call apply_indent('', comment, indent_level, output(i))
                in_continuation = .false.
                prev_content = content
                cycle
            end if

            call mask_strings(content, masked)

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
                call apply_indent(content, comment, indent_level + 1, output(i))
            else
                call apply_indent(content, comment, indent_level, output(i))
            end if

            ! Increase indent after emitting open keywords.
            if (opens > 0 .and. .not. in_continuation) then
                indent_level = indent_level + opens
            end if

            prev_content = content
        end do
    end subroutine format_lines

    ! Format a file in place. Returns exitcode 0 on success, 1 on I/O error.
    subroutine format_file(filepath, exitcode)
        character(len=*), intent(in) :: filepath
        integer, intent(out) :: exitcode

        character(len=MAX_LINE_LEN), allocatable :: lines(:), output(:)
        integer :: n_lines, n_output, u, ios, i

        exitcode = 0
        allocate (lines(MAX_LINES), output(MAX_LINES))

        n_lines = 0
        open (newunit=u, file=trim(filepath), status='old', &
            action='read', iostat=ios)
        if (ios /= 0) then
            exitcode = 1
            return
        end if
        do
            if (n_lines >= MAX_LINES) exit
            n_lines = n_lines + 1
            read (u, '(a)', iostat=ios) lines(n_lines)
            if (ios /= 0) then
                n_lines = n_lines - 1
                exit
            end if
        end do
        close (u)

        call format_lines(lines, n_lines, output, n_output)

        open (newunit=u, file=trim(filepath), status='replace', &
            action='write', iostat=ios)
        if (ios /= 0) then
            exitcode = 1
            return
        end if
        do i = 1, n_output
            write (u, '(a)') trim(output(i))
        end do
        close (u)
    end subroutine format_file

    ! Split a source line into content (stripped of leading whitespace) and
    ! the trailing comment (starting with '!', including the '!').
    ! String literals protect '!' characters inside them from being misread.
    subroutine split_line(line, content, comment)
        character(len=*), intent(in) :: line
        character(len=*), intent(out) :: content, comment

        integer :: i, n
        logical :: in_single, in_double
        character(len=1) :: ch

        comment = ''
        n = len_trim(line)
        in_single = .false.
        in_double = .false.

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
                return
            end if
        end do

        content = adjustl(line)
        call rstrip(content)
    end subroutine split_line

    ! Replace string literal contents with spaces (so keyword detection
    ! ignores text inside quotes).
    subroutine mask_strings(line, masked)
        character(len=*), intent(in) :: line
        character(len=*), intent(out) :: masked

        integer :: i, n
        logical :: in_single, in_double
        character(len=1) :: ch

        masked = line
        n = len_trim(line)
        in_single = .false.
        in_double = .false.

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

if (kw_starts(low, 'subroutine ')) then
    opens = 1; return
end if

! function: may start with type prefix (e.g. 'real function foo(...)')
if (has_function_opener(low)) then
    opens = 1; return
end if
end subroutine classify_line

! Apply indent to content and comment, write result to out_line.
subroutine apply_indent(content, comment, level, out_line)
    character(len=*), intent(in) :: content, comment
    integer, intent(in) :: level
    character(len=*), intent(out) :: out_line

    integer :: nsp
    character(len=4*64) :: spaces

    nsp = level*INDENT_WIDTH
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
    continuation_pending = (n > 0 .and. content(n:n) == '&')
end function continuation_pending

! Case-insensitive tolower for ASCII.
pure function to_lower(s) result(r)
    character(len=*), intent(in) :: s
    character(len=len(s)) :: r
    integer :: i, c
    r = s
    do i = 1, len(s)
        c = iachar(s(i:i))
        if (c >= 65 .and. c <= 90) r(i:i) = achar(c + 32)
    end do
end function to_lower

! True if the lowercased line starts with kw followed by space, '(', or EOL.
pure logical function kw_starts(line, kw)
    character(len=*), intent(in) :: line, kw
    integer :: n
    n = len(kw)
    kw_starts = (len_trim(line) >= n .and. line(1:n) == kw)
end function kw_starts

! True if the entire (trimmed) lowercased line equals kw or kw+optional name.
pure logical function kw_eq(line, kw)
    character(len=*), intent(in) :: line, kw
    integer :: n
    n = len(kw)
    kw_eq = (len_trim(line) == n .and. line(1:n) == kw) .or. &
        (len_trim(line) > n .and. line(1:n) == kw .and. &
        line(n + 1:n + 1) == ' ')
end function kw_eq

! True if trimmed line exactly equals kw, or starts with 'kw ' or 'kw('.
pure logical function matches(line, kw)
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

! True if 'if (...) then' block form (not single-line if).
logical function has_keyword_then(low)
    character(len=*), intent(in) :: low
    integer :: n
    n = len_trim(low)
    has_keyword_then = .false.
    ! Look for 'then' at end of line (possibly after closing paren/space).
    if (n >= 4 .and. low(n - 3:n) == 'then') then
        has_keyword_then = .true.
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
    integer :: pos
    has_function_opener = .false.
    pos = index(low, 'function ')
    if (pos > 0) then
        ! Exclude 'end function'
        if (pos >= 5 .and. low(pos - 4:pos - 1) == 'end ') return
        has_function_opener = .true.
    end if
end function has_function_opener

end module fo_format
